#!/usr/bin/env python3

import sys
import os
import datetime
import logging
import subprocess
import threading
import queue
import socket
import time
import argparse
from pathlib import Path
import shutil
from pprint import pprint
from datetime import datetime
from colorama import init, Fore, Back, Style

VERSION = "V1R0M0"
CODENAME = 'UNEXPECTED SLOTH'

error_check = [
                'open error',
                'Creating crash dump',
                'DISASTROUS ERROR',
                'HHC01023W Waiting for port 3270 to become free for console connections',
                'disabled wait state 00020000 80000005'
              ]

logname='sysgen.log'


usermods = ["AY12275","JLM0001","JLM0002","JLM0003","JLM0004","SLB0002","SYZM001","TIST801","TJES801", #usermods1.jcl
            "TMVS804","TMVS816","TTSO801","VS49603","WM00017","ZP60001","ZP60002","ZP60003","ZP60004", #usermods2.jcl
            "ZP60005","ZP60006","ZP60007","ZP60008","ZP60009","ZP60011","ZP60012","ZP60013","ZP60014", #usermods3.jcl
            "ZP60015","ZP60016","ZP60017","ZP60018","ZP60019","ZP60020","ZP60021","ZP60022","ZP60026", #usermods4.jcl
            "ZP60027","ZP60028","ZP60029","ZP60030","ZP60031","ZP60032","ZP60033","ZP60034","ZP60035", #usermods5.jcl
            "ZP60036","ZP60037","ZP60038","ZP60039","ZUM0007","ZUM0008",                               #usermods6.jcl
            "SYZJ2001",#"DYNPROC"
            ]

reply_num = 0

init(autoreset=True)
quit_herc_event = threading.Event()
kill_hercules = threading.Event()
reset_herc_event = threading.Event()
STDERR_to_logs = threading.Event()
running_folder = os.path.dirname(os.path.abspath(__file__)) + "/"
os.chdir(os.path.dirname(sys.argv[0]))

logging.basicConfig(filename=running_folder+logname,
                            filemode='w',
                            format='%(asctime)s,%(msecs)d %(name)s %(levelname)s %(message)s',
                            datefmt='%H:%M:%S',
                            level=logging.DEBUG)


USERJOB = ('''//{usern}    JOB (1),'ADD TSO USERS',CLASS=S,MSGLEVEL=(1,1),
//             MSGCLASS=A
// EXEC TSONUSER,ID={usern},
//      PW='{passwd}',
//      PR='{proc}',
//      OP='{oper}',
//      AC='{acct}',
//      JC='{jcl}',
//      MT='{mount}'
''')

class sysgen:
    ''' sysgen class to build hercules '''
    def __init__(self,
                 hercbin='hercules',
                 config='sysgen.conf',
                 version=False,
                 username=False,
                 password=False,
                 timeout=False,
                 no_compress=False,
                 users=False,
                 profiles=False,
                 keeptemp=False,
                 keepbackup=False,
                 install_path = "MVSCE/DASD"
                ):

        self.herccmd = hercbin
        logging.debug("herccmd set to {}".format(hercbin))
        self.configs = {}
        self.version = version
        self.username = username
        self.password = password
        self.no_compress = no_compress
        self.users = users
        self.profiles = profiles
        self.keeptemp = keeptemp
        self.keepbackup = keepbackup

        self.path = Path(running_folder+install_path)
        self.timeout = timeout

        if self.version:
            logging.debug("Version set to {}".format(version))

        if self.username:
            logging.debug("Username set to {}".format(username))

        if self.password:
            logging.debug("Password set to {}".format(password))

        if self.no_compress:
            logging.debug("DASD compression disabled")

        if self.users:
            logging.debug("users files set to {}".format(users))

        if self.profiles:
            logging.debug("profile file set to {}".format(profiles))

        if self.timeout:
            logging.debug("Timeout set to {}".format(timeout))

        try:
            os.remove('prt00e.txt')
            logging.debug('Removed prt00e.txt')
        except OSError:
            pass
        #self.print("Creating MVSCE folder if it does not exist")
        Path(running_folder+"MVSCE").mkdir(parents=True, exist_ok=True)
        Path(running_folder+"backup").mkdir(parents=True, exist_ok=True)
        Path(running_folder+"temp").mkdir(parents=True, exist_ok=True)

        self.print("Reading config file: {}".format(running_folder+config))
        self.read_configs(running_folder+config)
        self.hercproc = False
        # self.print("Starting Hercules")

        # self.hercproc = subprocess.Popen([self.herccmd, '--externalgui'],
        #                   stdin=subprocess.PIPE,
        #                   stdout=subprocess.PIPE,
        #                   stderr=subprocess.PIPE,
        #                   universal_newlines=True)

        self.stderr_q = queue.Queue()
        self.stdout_q = queue.Queue()
        self.step = ''
        self.substep = ''
        # self.printer_q = queue.Queue()

        #self.start_threads()

        # self.rc = self.hercproc.poll()
        # if self.rc is not None:
        #     raise("Unable to start hercules")

        # while True:
        #     try:
        #         line = self.stdout_q.get(False)
        #         #print( line.decode().strip())
        #         if "HHC01413I" in line:
        #             if line.split(' ')[3][0] != '4':
        #                 raise 'Hercules version 4.4+ required'
        #             else:
        #                 self.print("Using {}".format( line.strip()[10:]), color=Fore.GREEN)

        #         if "HHC01541I" in line:
        #             break

        #         #logging.debug(line.strip())
        #     except queue.Empty:
        #         continue

        # self.print("Hercules launched")
        # self.set_configs('generic')
        # #self.write_logs()
        # self.print("Hercules Initialization Complete")

    def install(self, step, substep):

        logging.debug("Install: step/substep {}/{}".format(step,substep))

        self.skip_steps = False
        if step:
            self.skip_steps = True

        try:
            if not step or step == "step_01_build_starter":
                self.step_01_build_starter()
                step = "step_02_install_smp4"

            if step == 'step_02_install_smp4':
                self.step_02_install_smp4()
                step = "step_03_build_dlibs"

            if step == 'step_03_build_dlibs':
                self.step_03_build_dlibs(substep)
                substep = False
                step = "step_04_system_generation"

            if step == 'step_04_system_generation':
                self.step_04_system_generation(substep)
                substep = False
                step = "step_05_usermods"

            if step == 'step_05_usermods':
                self.step_05_usermods(substep)
                substep = False
                step = "step_06_fdz1d02"

            if step == 'step_06_fdz1d02':
                self.step_06_fdz1d02()
                step = "step_07_customization"

            if step == 'step_07_customization':
                self.step_07_customization(substep)
                substep = False
                step = "step_08_rakf"

            if step == 'step_08_rakf':
                self.step_08_rakf()
                step = "step_09_cleanup"

            if step == 'step_09_cleanup':
                self.step_09_cleanup()

        finally:
            s, ss = self.get_step()
            if s and not ss:
                self.print("Install terminated at step {}. Use '-C' to restart at this step.".format(s),color=Fore.RED)
            elif s and ss:
                self.print("Install terminated at step/substep {}/{}. Use '-C' to restart at this step.".format(s,ss),color=Fore.RED)
            self.quit_hercules()

    def kill(self):
        self.hercproc.kill()

    def start_threads(self):
        # start a pair of threads to read output from hercules
        self.stdout_thread = threading.Thread(target=self.queue_stdout, args=(self.hercproc.stdout,self.stdout_q))
        self.stderr_thread = threading.Thread(target=self.queue_stderr, args=(self.hercproc.stderr,self.stderr_q))
        self.check_hercules_thread = threading.Thread(target=self.check_hercules, args=[self.hercproc])
        # self.queue_printer_thread = threading.Thread(target=self.queue_printer, args=('prt00e.txt',self.printer_q))
        self.stdout_thread.daemon = True
        self.stderr_thread.daemon = True
        # self.queue_printer_thread.daemon = True
        self.check_hercules_thread.daemon = True
        self.stdout_thread.start()
        self.stderr_thread.start()
        self.check_hercules_thread.start()
        # self.queue_printer_thread.start()

    def queue_stdout(self, pipe, q):
        ''' queue the stdout in a non blocking way'''
        global reply_num
        while True:

            l = pipe.readline()
            if len(l.strip()) > 0:
                if len(l.strip()) > 3 and l[0:2] == '/*' and l[2:4].isnumeric():
                    reply_num = l[2:4]
                    logging.debug("Reply number set to {}".format(reply_num))
                if  "HHC90020W" not in l and "HHC00007I" not in l and "HHC00107I" not in l and "HHC00100I" not in l:
                    # ignore these messages, they're just noise
                    # HHC90020W 'hthread_setschedparam()' failed at loc=timer.c:193: rc=22: Invalid argument
                    # HHC00007I Previous message from function 'hthread_set_thread_prio' at hthreads.c(1170)
                    logging.debug("[HERCLOG] {}".format(l.strip()))
                    q.put(l)
                    for errors in error_check:
                        if errors in l:
                            print("Quiting! Irrecoverable Hercules error: {}".format(l.strip()))
                            kill_hercules.set()
            if reset_herc_event.is_set():
                break

    def queue_stderr(self, pipe, q):
        ''' queue the stderr in a non blocking way'''
        while True:
            l = pipe.readline()
            if len(l.strip()) > 0:
                if STDERR_to_logs.is_set():
                    logging.debug("[DIAG] {}".format(l.strip()))
                if 'MIPS' in l:
                    logging.debug("[DIAG] {}".format(l.strip()))
                q.put(l)

                for errors in error_check:
                    if errors in l:
                        print("Quiting! Irrecoverable Hercules error: {}".format(l.strip()))
                        kill_hercules.set()
            if reset_herc_event.is_set():
                break

    def check_hercules(self, hercproc):
        ''' check to make sure hercules is still running '''
        while hercproc.poll() is None:
            if quit_herc_event.is_set() or reset_herc_event.is_set():
                logging.debug("Quit Event enabled exiting hercproc monitoring")
                return
            if kill_hercules.is_set():
                hercproc.kill()
                break
            continue

        self.print("ERROR - Hercules Exited Unexpectedly", color=Fore.RED)
        os._exit(1)

    def print(self, text='', color=Fore.WHITE):
        print(Style.BRIGHT+ "[+] " + color + text, flush=True)
        logging.debug(text)

    def send_herc(self, command=''):
        ''' Sends hercules commands '''
        logging.debug("Sending Hercules Command: {}".format(command))
        self.hercproc.stdin.write(command+"\n")
        self.hercproc.stdin.flush()

    def send_oper(self, command=''):
        ''' Sends operator/console commands (i.e. prepends /) '''
        self.send_herc("/{}".format(command))

    def send_reply(self, command=''):
        ''' Sends operator/console commands with automated number '''
        self.send_herc("/r {},{}".format(reply_num,command))

    def read_configs(self, config_file=''):
        logging.debug("Reading {}".format(config_file))
        ''' Reads the config file and populates self.configs '''
        with open(config_file, 'r') as config:
            for line in config.readlines():
                l = line.strip()
                if len(l) > 2 and "## SECTION:" in l:
                    section = l.split()[2]
                    self.configs[section] = []
                elif (len(l) > 0 and l[0] == "#") or len(l) == 0:
                    continue
                else:
                    self.configs[section].append(line.strip())

    def set_configs(self, config_section='generic'):
        logging.debug("Setting Hercules options")

        for config_item in self.configs[config_section]:
            if config_item.startswith("0"):
                #self.send_herc('detach {}'.format(config_item.split()[0]))
                self.send_herc('attach {}'.format(config_item))
            else:
                self.send_herc(config_item)

        self.wait_for_string(config_item)

    def unset_configs(self, config_section='generic'):
        logging.debug("Dettaching Hercules interfaces")

        for config_item in self.configs[config_section]:
            if config_item.startswith("0"):
                self.send_herc('detach {}'.format(config_item.split()[0]))
        #self.wait_for_string('detach {}'.format(config_item.split()[0]))

    def set_step(self, step, substep=False):
        logging.debug('Setting step to {} Setting substep to {}'.format(step,substep))
        self.step = step
        self.substep = substep
        with open(".step", 'w') as outfile:
            if substep:
                outfile.write("{} {}".format(step,substep))
            else:
                outfile.write("{}".format(step))

    def get_step(self):
        return self.step, self.substep

    def devinit(self, dev, upfile):
        self.send_herc("devinit {} {}{}".format(dev, running_folder,upfile))

    def step_01_build_starter(self):
        self.print("Step 1. Building Starter System",color=Fore.CYAN)
        self.set_step("step_01_build_starter")
        if os.path.exists(self.path):
            shutil.rmtree(self.path)
        self.print("Creating MVSCE/DASD folder if it does not exist")
        self.path.mkdir(parents=True, exist_ok=True)

        self.reset_hercules()
        STDERR_to_logs.set()
        self.dasdinit('starter')
        self.set_configs('build_starter')
        self.send_herc("ipl 280")
        self.send_herc("/")
        self.wait_for_string('HHC00010A Enter input for console 0:0009')
        self.print("[1/4] DASDI Initialization of the START1 DASD volume")
        logging.debug("Submitting instart1.sajob")
        self.send_oper("input=1442,00c")
        self.wait_for_string('/IBC154A  READY READER 00C.  DEPRESS INTERRUPT KEY.')
        self.wait_for_psw('1111')
        logging.debug("instart1.sajob complete")

        self.send_herc('stop')
        self.send_herc("ipl 280")
        self.wait_for_psw('FFFF')
        self.send_herc("/")
        self.wait_for_string('HHC00010A Enter input for console 0:0009')
        self.print("[2/4] Performing restore of the START1 DASD volume")
        logging.debug("Submitting rsstart1.sajob")
        self.send_oper("input=1442,00d")
        self.wait_for_psw('EEEE')
        logging.debug("rsstart1.sajob complete")

        self.send_herc('stop')
        self.send_herc("ipl 281")
        self.wait_for_psw('FFFF')
        self.send_herc("/")
        self.wait_for_string('HHC00010A Enter input for console 0:0009')
        self.print("[3/4] DASDI Initialization of the SPOOL0 DASD volume")
        logging.debug("Submitting inspool0.sajob")
        self.send_oper("input=1442,00e")
        self.wait_for_psw('1111')
        logging.debug("inspool0.sajob complete")

        self.send_herc('stop')
        self.send_herc("ipl 281")
        self.wait_for_psw('FFFF')
        self.send_herc("/")
        self.wait_for_string('HHC00010A Enter input for console 0:0009')
        self.print("[4/4] Performing restore of the SPOOL0 DASD volume")
        logging.debug("Submitting rsspool0.sajob")
        self.send_oper("input=1442,00f")
        self.wait_for_psw('EEEE')
        logging.debug("rsspool0.sajob complete")
        self.send_herc('stop')
        self.quit_hercules(msg=False)
        #self.unset_configs('build_starter')
        # Wait for the last item to be detached before continuing
        #self.wait_for_string("HHC01603I detach {}".format(self.configs['build_starter'][-1].split()[0]))
        self.backup_dasd("01_build_starter")

        self.print("Build Starter System Complete",color=Fore.GREEN)
        STDERR_to_logs.clear()



    def step_02_install_smp4(self):

        self.print("Step 2. Using SMP4 to Build the Distribution Libraries",color=Fore.CYAN)

        self.set_step("step_02_install_smp4")

        self.restore_dasd("01_build_starter")
        self.reset_hercules()
        self.dasdinit('distribution_libs')
        self.set_configs('smp1')
        self.wait_for_string("0:0151 CKD")
        self.print("Installing SMP 4.44 on the Starter System")
        self.send_herc("ipl 150")
        self.wait_for_string("HHC00010A Enter input for console 0:001F")
        self.send_oper('r 0,clpa')
        self.wait_for_string("00 $HASP426 SPECIFY OPTIONS - HASP-II, VERSION JES2 4.0")
        self.print("Formatting SPOOL0")
        self.send_oper('r 0,format,noreq')
        self.wait_for_string('$HASP436 REPLY Y OR N TO CONFIRM CHECKPOINT RECORD CHANGE')
        self.send_reply("y")
        self.wait_for_string('$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE')
        self.send_oper('$pi2-3')
        self.wait_for_string('IS PURGED')
        self.print('System Online, mounting DASD')
        self.send_herc('attach 148 3350 MVSCE/DASD/smp000.3350')
        self.send_herc('attach 149 3350 MVSCE/DASD/work00.3350')
        self.send_herc('attach 14a 3350 MVSCE/DASD/work01.3350')
        self.wait_for_string('HHC00414I 0:0148 CKD file')
        self.print("Installing SMP 4.44")
        self.send_herc("devinit 12 jcl/smp4p44.jcl")
        self.wait_for_string('HHC02245I 0:0012 device initialized')
        self.send_herc('devinit 170 tape/zdlib1.het')
        self.wait_for_string("IEF238D SMP4P44 - REPLY DEVICE NAME OR 'CANCEL'.")
        self.send_reply('170')
        self.wait_for_string('IEC501A SMP4P44,S2')
        self.send_herc('devinit 170 tape/smp4b.het')
        self.wait_for_string("IEC507D REPLY 'U'-USE OR 'M'-UNLOAD")
        self.send_reply('u')
        self.wait_for_string("IEC507D REPLY 'U'-USE OR 'M'-UNLOAD")
        self.send_reply('u')
        self.wait_for_string("IEC507D REPLY 'U'-USE OR 'M'-UNLOAD")
        self.send_reply('u')
        self.print("Initializing WORK00, WORK01 and SMP000")
        self.wait_for_string("IEH841D 148    CONFIRM REQUEST TO INITIALIZE")
        self.send_reply('u')
        self.wait_for_string('IEH841D 149    CONFIRM REQUEST TO INITIALIZE')
        self.send_reply('u')
        self.wait_for_string('IEH841D 14A    CONFIRM REQUEST TO INITIALIZE')
        self.send_reply('u')
        self.wait_for_string('$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE')
        self.print("SMP 4.44 Install Complete",color=Fore.GREEN)

        self.check_maxcc(jobname='SMP4P44')
        self.shutdown_mvs()
        #self.wait_for_string('HHC01603I detach 014A')
        self.quit_hercules(msg=True)
        self.backup_dasd("02_install_smp4")
        if quit:
            self.quit_hercules()

    def step_03_build_dlibs(self, start=False):
        '''macro function to run the various steps
            The start variable allows you to skip to specific steps'''
        logging.debug("step_03_build_dlibs: starting at step {}".format(start))
        self.skip_steps = False
        self.set_step("step_03_build_dlibs")

        self.print("Step 3. Building the MVS 3.8j Distribution Libraries",color=Fore.CYAN)
        if not start or start == "smpmount":
            self.smpmount()
            start = "smpjob00"

        if start == 'smpjob00':
            self.smpjob00()
            start = "smpjob01"

        if start == 'smpjob01':
            self.smpjob01()
            start = "smpjob02"

        if start == 'smpjob02':
            self.smpjob02()
            start = "smpjob03"

        if start == 'smpjob03':
            self.smpjob03()
            start = "smpjob04"

        if start == 'smpjob04':
            self.smpjob04()
            start = "smpjob06"

        if start == 'smpjob06':
            self.smpjob06()
            start = "smpjob07"

        if start == 'smpjob07':
            self.smpjob07()

        self.print("Building the MVS 3.8j Distribution Libraries Complete",color=Fore.GREEN)

    def smpmount(self, quit=False):
        self.set_step("step_03_build_dlibs","smpmount")
        self.restore_dasd("02_install_smp4")
        self.reset_hercules()
        self.set_configs('smp2')
        self.send_herc('detach 0012')
        self.send_herc('attach 0012    3505    jcl/smpmount.jcl eof')
        self.send_herc("ipl 150")
        self.wait_for_string("HHC00010A Enter input for console 0:001F")
        self.send_herc('/')
        self.wait_for_string("$HASP426 SPECIFY OPTIONS - HASP-II, VERSION JES2 4.0")
        self.send_oper('r 0,noreq')
        self.wait_for_string("IEF166D REPLY Y/N TO EXECUTE/SUPPRESS COMMAND")
        self.print("Assingning the volume SMP000 to the class of PRIVATE")
        self.send_reply('y')
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")

        self.check_maxcc(jobname='SMPMOUNT')
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("03_SMPMOUNT")

    def smpjob00(self):
        self.set_step("step_03_build_dlibs","smpjob00")
        self.restore_dasd("03_SMPMOUNT")
        if self.skip_steps:
            self.print("Step 3. Building the MVS 3.8j Distribution Libraries",color=Fore.CYAN)
        self.reset_hercules()
        self.set_configs('smp2')
        self.smpjobs_ipl('Allocating and initializing required datasets')
        self.send_herc("devinit 12 jcl/smpjob00.jcl")
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        # The first step - IEHPROGM - receives a condition code of 0008 because it is attempting to delete datasets that are not present.
        # The second step - IEFBR14 - is pre-allocating datasets.
        # The third step - SMP - is initializing control information in several of the SMP datasets.
        # One of the first tasks it attempts is to delete a target that is not there (remember the
        # datasets were just allocated and are empty), so it gets a condition code of 0008.  It will
        # always get that code, so it is expected and acceptable.
        self.check_maxcc(jobname='SMPJOB00', steps_cc={'IEHPROGM':'0008', 'DLBUCL' : '0008' })
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("04_SMPJOB00")

    def smpjob01(self):
        self.set_step("step_03_build_dlibs","smpjob01")
        self.restore_dasd("04_SMPJOB00")
        if self.skip_steps:
            self.print("Step 3. Building the MVS 3.8j Distribution Libraries",color=Fore.CYAN)
        self.reset_hercules()
        self.set_configs('smp2')
        self.smpjobs_ipl('Loading MVS 3.8j product elements into SMP from tape')
        self.send_herc("devinit 12 jcl/smpjob01.jcl")
        self.wait_for_string("IEF247I SMPJOB01 - 471,570,571,670,671 NOT ACCESSIBLE")
        self.send_herc('devinit 170 tape/zdlib1.het')
        self.send_reply('170')
        self.wait_for_string('$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE')
        self.check_maxcc(jobname='SMPJOB01')
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("05_SMPJOB01")

    def smpjob02(self):
        self.set_step("step_03_build_dlibs","smpjob02")
        self.restore_dasd("05_SMPJOB01")
        if self.skip_steps:
            self.print("Step 3. Building the MVS 3.8j Distribution Libraries",color=Fore.CYAN)

        self.reset_hercules()
        self.set_configs('smp2')
        self.smpjobs_ipl('Copying 1,482 PTFs into SMP datasets')
        self.send_herc("devinit 12 jcl/smpjob02.jcl")
        self.wait_for_string("IEF247I SMPJOB02 - 471,570,571,670,671 NOT ACCESSIBLE")
        self.send_herc('devinit 170 tape/ptfs.het')
        self.send_reply('170')
        self.wait_for_string('$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE')
        self.check_maxcc(jobname='SMPJOB02')
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("06_SMPJOB02")

    def smpjob03(self):
        self.set_step("step_03_build_dlibs","smpjob03")
        self.restore_dasd("06_SMPJOB02")
        if self.skip_steps:
            self.print("Step 3. Building the MVS 3.8j Distribution Libraries",color=Fore.CYAN)

        self.reset_hercules()
        self.set_configs('smp2')
        self.print("Updating SMP 4.44 to SMP 4.48")
        self.smpjobs_ipl('Accepting product elements and PTFs')
        self.send_herc("devinit 12 jcl/smpjob03.jcl")
        self.print("  !!This step can take upwards of twenty minutes!!")
        self.wait_for_string('$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE', timeout=3600)
        self.check_maxcc(jobname='SMPJOB03', steps_cc={"DLBUCL2":"0004", "DLBUCL4":"0004" })
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("07_SMPJOB03")

    def smpjob04(self):
        self.set_step("step_03_build_dlibs","smpjob04")
        self.restore_dasd("07_SMPJOB03")
        if self.skip_steps:
            self.print("Step 3. Building the MVS 3.8j Distribution Libraries",color=Fore.CYAN)

        self.reset_hercules()
        self.set_configs('smp2')
        self.smpjobs_ipl('Installing Jim Morrison Usermods for 3375, 3380, and 3390 DASD devices')
        self.send_herc("devinit 12 jcl/smpjob04.jcl")
        self.wait_for_string("IEF247I SMPJOB04 - 471,570,571,670,671 NOT ACCESSIBLE")
        self.send_herc("devinit 170 tape/j90009.het")
        self.send_reply('170')
        self.wait_for_string('$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE')
        self.check_maxcc(jobname='SMPJOB04')
        self.send_herc("devinit 12 jcl/smpjob05.jcl")
        self.wait_for_string('$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE')
        self.check_maxcc(jobname='SMPJOB05', steps_cc={"DLBUCL":"0004"})
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("08_SMPJOB04-05")

    def smpjob06(self):
        self.set_step("step_03_build_dlibs","smpjob06")
        self.restore_dasd("08_SMPJOB04-05")
        if self.skip_steps:
            self.print("Step 3. Building the MVS 3.8j Distribution Libraries",color=Fore.CYAN)

        self.reset_hercules()
        self.set_configs('smp2')
        self.smpjobs_ipl('Cleaning up')
        self.send_herc("devinit 12 jcl/smpjob06.jcl")
        self.wait_for_string('$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE')
        self.check_maxcc(jobname='SMPJOB06', steps_cc={"DLBUCL":"0004"})
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("09_SMPJOB06")

    def smpjob07(self):
        self.set_step("step_03_build_dlibs","smpjob07")
        self.restore_dasd("09_SMPJOB06")
        if self.skip_steps:
            self.print("Step 3. Building the MVS 3.8j Distribution Libraries",color=Fore.CYAN)

        self.reset_hercules()
        self.set_configs('smp2')
        self.smpjobs_ipl('Building ICKDSF Utility and Re-Linking IFOX00')
        self.send_herc("devinit 12 jcl/smpjob07.jcl")
        self.wait_for_string("IEC507D REPLY 'U'-USE OR 'M'-UNLOAD")
        self.send_reply('u')
        self.wait_for_string("IEC507D REPLY 'U'-USE OR 'M'-UNLOAD")
        self.send_reply('u')
        self.wait_for_string("IEC507D REPLY 'U'-USE OR 'M'-UNLOAD")
        self.send_reply('u')
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.check_maxcc(jobname='SMPJOB07')
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("10_SMPJOB07")

    def smpjobs_ipl(self, step_text=''):
        self.print(step_text)
        self.send_herc('detach 0012')
        self.send_herc('attach 0012    3505    jcl/null.jcl eof')
        self.send_herc("ipl 150")
        self.wait_for_string("HHC00010A Enter input for console 0:001F")
        self.send_herc('/')
        self.wait_for_string("$HASP426 SPECIFY OPTIONS - HASP-II, VERSION JES2 4.0")
        self.send_oper('r 0,noreq')
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")

    # SYSGEN

    def step_04_system_generation(self, start=False):
        '''macro function to run the various steps
            The start variable allows you to skip to specific steps'''
        logging.debug("step_04_system_generation: starting at step {}".format(start))

        self.set_step("step_04_system_generation")
        self.skip_steps = False
        self.print("Step 4. Performing a System Generation - Building MVS 3.8j",color=Fore.CYAN)

        if not start or start == 'sysgen00':
            self.sysgen00()
            start = "sysgen01"

        if start == 'sysgen01':
            self.sysgen01()
            start = "sysgen01a"

        if start == 'sysgen01a':
            self.sysgen01a()
            start = "sysgen01b"

        if start == 'sysgen01b':
            self.sysgen01b()
            start = "sysgen01c"

        if start == 'sysgen01c':
            self.sysgen01c()
            start = "sysgen01d"

        if start == 'sysgen01d':
            self.sysgen01d()
            start = "sysgen01e"

        if start == 'sysgen01e':
            self.sysgen01e()
            start = "sysgen01f"

        if start == 'sysgen01f':
            self.sysgen01f()
            start = "sysgen02"

        if start == 'sysgen02':
            self.sysgen02()
            start = "sysgen03"

        if start == 'sysgen03':
            self.sysgen03()
            start = "sysgen04"

        if start == 'sysgen04':
            self.sysgen04()
            start = "sysgen05"

        if start == 'sysgen05':
            self.sysgen05()
            start = "sysgen05a"

        if start == 'sysgen05a':
            self.sysgen05a()
            start = "sysgen06"

        if start == 'sysgen06':
            self.sysgen06()

        self.print("System Generation - Building MVS 3.8j Complete",color=Fore.GREEN)

    def sysgenjobs_ipl(self, step_text=''):
        self.print(step_text)
        self.reset_hercules()
        self.set_configs('sysgen2')
        #self.wait_for_string("0:0151 CKD")
        self.send_herc("ipl 150")
        self.wait_for_string("HHC00010A Enter input for console 0:001F")
        self.send_oper('r 0,clpa')
        self.wait_for_string("00 $HASP426 SPECIFY OPTIONS - HASP-II, VERSION JES2 4.0")
        self.send_oper('r 0,noreq')
        self.wait_for_string('$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE')
        # self.send_oper('$pi2-3')
        # self.wait_for_string('$HASP250 INIT     IS PURGED')
        # self.wait_for_string('$HASP250 INIT     IS PURGED')
        # self.wait_for_string('$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE')

    def sysgen00(self):
        self.set_step("step_04_system_generation","sysgen00")

        self.restore_dasd("10_SMPJOB07")
        if self.skip_steps:
            self.print("Step 4. Performing a System Generation - Building MVS 3.8j",color=Fore.CYAN)

        self.reset_hercules()
        self.dasdinit('sysgen')
        self.set_configs('sysgen')
        #self.wait_for_string("0:0151 CKD")
        self.send_herc("ipl 150")
        self.wait_for_string("HHC00010A Enter input for console 0:001F")
        self.send_oper('r 0,clpa')
        self.wait_for_string("00 $HASP426 SPECIFY OPTIONS - HASP-II, VERSION JES2 4.0")
        self.send_oper('r 0,noreq')
        self.wait_for_string('$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE')
        self.send_oper('$pi2-3')
        self.wait_for_string('IS PURGED')
        #self.print('System Online, mounting DASD')
        self.send_herc('attach 149 3350 MVSCE/DASD/mvsres.3350')
        self.send_herc('attach 14a 3350 MVSCE/DASD/mvs000.3350')
        self.send_herc('attach 14b 3350 MVSCE/DASD/spool1.3350')
        self.send_herc('attach 14c 3350 MVSCE/DASD/page00.3350')
        self.wait_for_string('HHC00414I 0:014C CKD file')
        self.print("Initializing target DASD volumes and preparing for System Generation")
        self.send_herc("devinit 12 jcl/sysgen00.jcl")
        self.wait_for_string("ICK003D REPLY U TO ALTER VOLUME 149 CONTENTS")
        self.send_reply('u')
        self.wait_for_string("ICK003D REPLY U TO ALTER VOLUME 14A CONTENTS")
        self.send_reply('u')
        self.wait_for_string("ICK003D REPLY U TO ALTER VOLUME 14B CONTENTS")
        self.send_reply('u')
        self.wait_for_string("ICK003D REPLY U TO ALTER VOLUME 14C CONTENTS")
        self.send_reply('u')
        self.wait_for_string("IEF166D REPLY Y/N TO EXECUTE/SUPPRESS COMMAND")
        self.send_reply('y')
        self.wait_for_string("IEF166D REPLY Y/N TO EXECUTE/SUPPRESS COMMAND")
        self.send_reply('y')
        self.wait_for_string("IEF166D REPLY Y/N TO EXECUTE/SUPPRESS COMMAND")
        self.send_reply('y')
        self.wait_for_string("IEF166D REPLY Y/N TO EXECUTE/SUPPRESS COMMAND")
        self.send_reply('y')
        self.wait_for_string("IEF166D REPLY Y/N TO EXECUTE/SUPPRESS COMMAND")
        self.send_reply('y')
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.check_maxcc(jobname='MOUNT')
        self.check_maxcc(jobname='SYSGEN00', steps_cc={"IEHPROGM":"0008"})
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("11_SYSGEN00")

    def sysgen01(self):
        self.set_step("step_04_system_generation","sysgen01")

        self.restore_dasd("11_SYSGEN00")
        if self.skip_steps:
            self.print("Step 4. Performing a System Generation - Building MVS 3.8j",color=Fore.CYAN)

        self.sysgenjobs_ipl("Building the hardware configuration for MVS 3.8j")
        self.send_herc("devinit 12 jcl/sysgen01.jcl")
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.check_maxcc(jobname='SYSGEN01', steps_cc={"CLEANUP":"0008"})

        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("12_SYSGEN01")
        self.hetget_sysgen01()
        self.sysgen01_extract()

    def sysgen01a(self):
        self.set_step("step_04_system_generation","sysgen01a")
        if self.skip_steps:
            self.restore_dasd("12_SYSGEN01")
            self.print("Step 4. Performing a System Generation - Building MVS 3.8j",color=Fore.CYAN)

        self.sysgenjobs_ipl("Building SYSGEN01A")
        self.send_herc("devinit 12 temp/sysgen01a.jcl")
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        steps={
            "SG19": "0004",
            "SG28": "0004",
            "SG29": "0004",
            "SG31": "0004",
            "SG32": "0004",
            "SG37": "0004",
        }
        self.check_maxcc(jobname='SYSGEN1',steps_cc=steps)
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("13_SYSGEN01A")

    def sysgen01b(self):
        self.set_step("step_04_system_generation","sysgen01b")
        self.restore_dasd("13_SYSGEN01A")
        if self.skip_steps:
            self.print("Step 4. Performing a System Generation - Building MVS 3.8j",color=Fore.CYAN)

        self.sysgenjobs_ipl("Building SYSGEN01B")
        self.send_herc("devinit 12 temp/sysgen01b.jcl")
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        steps={
            "SG8": "0004",
            "SG13": "0004",
            "SG33": "0004"
        }
        self.check_maxcc(jobname='SYSGEN2',steps_cc=steps)
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("14_SYSGEN01B")

    def sysgen01c(self):
        self.set_step("step_04_system_generation","sysgen01c")
        self.restore_dasd("14_SYSGEN01B")
        if self.skip_steps:
            self.print("Step 4. Performing a System Generation - Building MVS 3.8j",color=Fore.CYAN)

        self.sysgenjobs_ipl("Building SYSGEN01C")
        self.send_herc("devinit 12 temp/sysgen01c.jcl")
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.check_maxcc(jobname='SYSGEN3')
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("15_SYSGEN01C")

    def sysgen01d(self):
        self.set_step("step_04_system_generation","sysgen01d")
        self.restore_dasd("15_SYSGEN01C")
        if self.skip_steps:
            self.print("Step 4. Performing a System Generation - Building MVS 3.8j",color=Fore.CYAN)

        self.sysgenjobs_ipl("Building SYSGEN01D")
        self.send_herc("devinit 12 temp/sysgen01d.jcl")
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.check_maxcc(jobname='SYSGEN4')
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("16_SYSGEN01D")

    def sysgen01e(self):
        self.set_step("step_04_system_generation","sysgen01e")
        self.restore_dasd("16_SYSGEN01D")
        if self.skip_steps:
            self.print("Step 4. Performing a System Generation - Building MVS 3.8j",color=Fore.CYAN)

        self.sysgenjobs_ipl("Building SYSGEN01E")
        self.send_herc("devinit 12 temp/sysgen01e.jcl")
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.check_maxcc(jobname='SYSGEN5')
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("17_SYSGEN01E")

    def sysgen01f(self):
        self.set_step("step_04_system_generation","sysgen01f")
        self.restore_dasd("17_SYSGEN01E")
        if self.skip_steps:
            self.print("Step 4. Performing a System Generation - Building MVS 3.8j",color=Fore.CYAN)

        self.sysgenjobs_ipl("Building SYSGEN01F")
        self.send_herc("devinit 12 temp/sysgen01f.jcl")
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        steps={
            "STEPZ1": "0008"
        }
        self.check_maxcc(jobname='SYSGEN6',steps_cc=steps)
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("18_SYSGEN01F")

    def sysgen02(self):
        self.set_step("step_04_system_generation","sysgen02")
        self.restore_dasd("18_SYSGEN01F")
        if self.skip_steps:
            self.print("Step 4. Performing a System Generation - Building MVS 3.8j",color=Fore.CYAN)

        self.sysgenjobs_ipl("Generating JES2")
        self.send_herc("devinit 12 jcl/sysgen02.jcl")
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.check_maxcc(jobname='SYSGEN02')
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("19_SYSGEN02")

    def sysgen03(self):
        self.set_step("step_04_system_generation","sysgen03")
        self.restore_dasd("19_SYSGEN02")
        if self.skip_steps:
            self.print("Step 4. Performing a System Generation - Building MVS 3.8j",color=Fore.CYAN)

        self.sysgenjobs_ipl("Creating SYS1.APVTMACS")
        self.send_herc("devinit 170 tape/apvtmacs.het")
        self.send_herc("devinit 12 jcl/sysgen03.jcl")
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.check_maxcc(jobname='SYSGEN03')
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("20_SYSGEN03")

    def sysgen04(self):
        '''
        This step does multiple things:

             - makes backup copies of SYS1.PARMLIB members that will be changed, creates
               member IEASYS00 in SYS1.PARMLIB, During IPL, the values coded in IEASYS00 are
               used to customize the MVS 3.8j environment.  Several of the parameters
               contained in IEASYS00 are simply pointers to other members of SYS1.PARMLIB.
               Many of the parameters may be overridden at IPL time, but the IPL process is
               greatly simplified if the most frequently desired settings are coded in this
               member.

             - creates member IEAAPF00 in SYS1.PARMLIB, IEAAPF00 specifies the load module
               libraries from which authorized programs may be executed.

             - creates member SMFPRM00 in SYS1.PARMLIB, SMFPRM00 contains parameters related
               to System Management Facility, which collects and records information about MVS
               and the programs that are executed under its control.

             - creates member IEALOD00 in SYS1.PARMLIB, IEALOD00 specifies load modules that
               contain part of the operating code for MVS which are to be loaded and fixed in
               real storage.  These modules are so frequently executed, fixing them in storage
               can improve the performance of the system.

             - creates COMMND00 in SYS1.PARMLIB, COMMND00 contains operator commands which are
               to be executed automatically upon the completion of the IPL.

             - creates SETPFK00 in SYS1.PARMLIB, SETPFK00 contains settings for the console
               Program Function Keys.  Commands associated with the PFKeys here may be
               executed simply by pressing the associated PFK on the MVS console.

             - creates LNKLST00 in SYS1.PARMLIB, LNKLST00 contains load module libraries which
               are to be automatically searched for load modules executed in batch or under
               TSO.

             - creates VATLST00 in SYS1.PARMLIB, VATLST00 specifies the Volume Attributes to
               be associated with all regularly used DASD volumes.

             - creates PARMTZ in SYS1.PARMLIB, PARMTZ specifies the offset from GMT to
               establish local time.  I have set mine up for the Central Time zone in the
               United States; you may wish to modify this for your geographic location before
               you submit this jobstream.

             - deletes SYS2.LOCAL.LPALIB, This library was created by SYSGEN00 to provide
               local SVC modules during System Generation.  It is no longer needed, so this is
               a good place to delete it.

             - allocates and catalogs SYS2.LINKLIB, SYS1.LINKLIB is the main load module
               library and is created during System Generation.  However, it is not a good
               idea to add your own load modules to that library, so we create SYS2.LINKLIB to
               have a place set up to receive any programs you write locally.

             - allocates and catalogs SYS2.PROCLIB, Likewise, SYS1.PROCLIB is the system's
               catalogued procedure library, but it is not a good idea to add your own
               catalogued procedures to that library, so we create SYS2.PROCLIB for local use.

             - allocates and catalogs SYS2.CMDLIB, As SYS1.LINKLIB is to batch load modules,
               SYS1.CMDLIB is to TSO load modules.  SYS2.CMDLIB is created to be a repository
               for your local TSO load modules.

             - allocates and catalogs SYS2.HELP, SYS2.HELP corresponds to SYS1.HELP -
               SYS2.HELP will contain 'help' (syntax and operand usage instructions) for your
               local TSO load modules.

             - creates member JES2 in SYS1.PROCLIB, Although a basic JES2 catalogued procedure
               member was created by SYSGEN02, a more customized one is created here,
               specifically to utilize/incorporate the SYS2.PROCLIB allocated (three bullets)
               above.

             - creates member CLEARDMP in SYS2.PROCLIB, There are three dump datasets
               allocated on MVSRES to receive system dumps when a catastrophic error occurs.
               If all three of these fill up (are used) the system will not be able to be
               started, so you need to be able to clear these datasets as they are used.  This
               procedure, which may be started from the console, clears and initializes dump
               datasets.

             - creates member CLEARERP in SYS2.PROCLIB, When hardware errors occur, an entry
               is made in SYS1.LOGREC (the environmental error recorder dataset).  This
               dataset will hold a finite number of entries.  This procedure, which may be
               started from the console, clears SYS1.LOGREC.

             - creates member COMPRESS in SYS2.PROCLIB, When library (Partitioned) datasets
               receive many updates, they frequently become in need of reorganization.  If
               left to the extreme, they may exhaust all of their extents.  This procedure,
               which may be started from the console, can be used to compress any partitioned
               dataset, reclaiming unused extents.

             - creates members SMPASM, SMPASML, SMPAPP, and SMPREC in SYS2.PROCLIB, These
               catalogued procedures are used to receive, assemble, assemble/link, and apply
               user modifications and PTFs.

             - copies the members created above - SMPASM, SMPASML, SMPAPP, and SMPREC - into
               SYS1.PROCLIB on START1. We need these to use the System Modification Program to
               update the target system from the MVS Starter System.

             - adds macro IHADVCT to SYS1.MACLIB. This macro acquired from one of the datasets
               on the MVS3.8j optional materials tapes  (MVS Optional Materials Tapes) is
               required for some USERMODs to assemble correctly.

             - allocates and catalogs SYS1.UMODMAC, SYS1.UMODCNTL, SYS1.UMODSRC, SYS1.UMODOBJ,
               and SYS1.UMODLIB, These datasets are used to hold user modifications.

             - allocates and catalogs SYS1.CDS, SYS1.CRQ, and SYS1.SMPCDS, These datasets are
               used to record and track modifications to the target system.

             - initializes SMP for target system's environment.

             - allocates and catalogs SYS2.CONTROL, Some catalogued procedures need control
               cards, and, since control cards cannot be included in catalogued procedures, we
               create this library to hold those type of datasets.

             - adds CLS TSO command and help text for the command, CLS is a short program
               (from CBT 430, file #300) that simply clears the screen for a TSO user.  It is
               compiled into SYS2.CMDLIB with the HELP text placed into SYS2.HELP.

             - adds SETPFKEY batch load module and an execution procedure, This program (from
               CBT 249, file #295) equates operator console commands to the Program Function
               Keys on MVS consoles.  It is compiled into SYS2.LINKLIB and a procedure to
               execute the program is placed into SYS2.PROCLIB.  In a previous step there was
               a START command set up for this program in COMMND00 and PFKey definitions were
               placed into SYS1.PARMLIB(SETPFK00).

             - adds ZTIMER batch load module, execution procedure, and parameter member, This
               program may be used to display reminders or execute MVS operator commands on a
               scheduled or periodic basis.  It is compiled into SYS2.LINKLIB and a procedure
               to execute the program is placed into SYS2.PROCLIB.  In a previous job there
               was a START command set up for this program in COMMND00.  A set of commands is
               placed into SYS2.CONTROL(ZTIMER) for use by the STARTed process to periodically
               clear JES2 queues of non-processed output.

             - adds CLIP batch load module and execution procedure, This program is used from
               the console to relabel (clip) offline DASD volumes.  It is compiled into
               SYS2.LINKLIB and a procedure to execute the program is placed into SYS2.PROCLIB.

             - creates a list of modules to fix in the Link Pack Area in SYS1.PARMLIB, The
               member containing the list is simply copied from another member supplied by IBM
               and could have been utilized as it was, but by creating a copy of the member
               contents, custom modifications may be made to the 'active' copy without losing
               the original set supplied by IBM.

             - submits a job to load into the SMP datasets the specifications for the
               generated MVS 3.8j system and JES2.

             - It is necessary to load the specifications of the new system (acquired from the
               Stage 2 jobstreams which were stored on a work DASD volume) and JES2 so that
               future modifications may be properly integrated into the system.
        '''

        self.set_step("step_04_system_generation","sysgen04")
        self.restore_dasd("20_SYSGEN03")
        if self.skip_steps:
            self.print("Step 4. Performing a System Generation - Building MVS 3.8j",color=Fore.CYAN)

        self.sysgenjobs_ipl("Creating system datasets")
        self.send_herc("devinit 12 jcl/sysgen04.jcl")
        self.wait_for_string("IEC507D REPLY 'U'-USE OR 'M'-UNLOAD")
        self.send_reply('U')
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.check_maxcc(jobname='SYSGEN04')
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("21_SYSGEN04")

    def sysgen05(self):
        '''

        This step does multiple things:
             - creates member IKJTSO00 in SYS1.PARMLIB, This member specifies parameters which
               control the execution of TSO.

             - creates member IKJACCNT in SYS1.PROCLIB, This member specifies parameters which
               define the individual TSO Users' address spaces.

             - creates member NET in SYS1.PROCLIB, This procedure is executed to start VTAM.

             - creates member TSO in SYS1.PROCLIB, This procedure is executed to start TSO.

             - allocates and catalogues SYS1.VTAMLST, This library will contain specifications
               for VTAM.

             - creates member ATCSTR00 in SYS1.VTAMLST, This member specifies VTAM parameters.

             - creates member ATCCON00 in SYS1.VTAMLST, This members specifies the tables to
               VTAM for TSO applications and local 3270 terminals.

             - creates member APPLTSO in SYS1.VTAMLST, This member specifies the logon table
               for all local 3270 terminals.  (8 defined initially, you would add more here if
               you need more 3270 terminals to use concurrently)

             - creates member LCL400 in SYS1.VTAMLST, This member specifies characteristics
               for all local 3270 terminals.  (8 defined initially, you would add more here if
               you need more 3270 terminals to use concurrently)

             - allocates and catalogues SYS1.VTAMSRC, This library will contain LOGON tables
               for VTAM.

             - creates member LOGTAB01 in SYS1.VTAMSRC, This member specifies the LOGON
               interpret table for VTAM.

             - creates member LOGMOD01 in SYS1.VTAMSRC, This member specifies the
               characteristics for all variants of local 3270 terminals (and VTAM controlled
               printers).

             - allocates and catalogues SYS1.CMDPROC, This library will contain TSO command
               procedures (CLISTs) available to all TSO users.

             - creates member TSOLOGON in SYS1.CMDPROC, Default TSO logon message.

             - creates member USRLOGON in SYS1.CMDPROC, Default TSO logon procedure.

             - creates TSONUSER procedure in SYS2.PROCLIB, This procedure is used to create a
               new TSO user's information record in the SYS1.UADS datasets and also allocates
               all the datasets required for a new TSO user.

             - creates TSODUSER procedure in SYS2.PROCLIB, This procedure is used to delete a
               TSO user's information record in the SYS1.UADS datasets and also (optionally)
               deletes all the datasets associated with the TSO user.
        '''
        self.set_step("step_04_system_generation","sysgen05")
        self.restore_dasd("21_SYSGEN04")
        if self.skip_steps:
            self.print("Step 4. Performing a System Generation - Building MVS 3.8j",color=Fore.CYAN)

        self.sysgenjobs_ipl("Creating system dataset members")
        self.send_herc("devinit 12 jcl/sysgen05.jcl")
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.check_maxcc(jobname='SYSGEN05')
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("22_SYSGEN05")

    def sysgen05a(self):
        ''' Adding version/release info to NETSOL and SYS1.PARMLIB(RELEASE)'''
        self.set_step("step_04_system_generation","sysgen05a")

        self.restore_dasd("22_SYSGEN05")
        if self.skip_steps:
            self.print("Step 4. Performing a System Generation - Building MVS 3.8j",color=Fore.CYAN)

        git_hash = ''
        try:
            git_query_path = subprocess.check_output(["which", "git"]).strip()
            version = subprocess.check_output([git_query_path, 'rev-parse', '--short', 'HEAD'], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL).decode('ascii').strip()
            git_hash = subprocess.check_output([git_query_path, 'rev-parse', 'HEAD'], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL).decode('ascii').strip()
        except:
            version = VERSION

        if self.version:
            version = self.version

        netsol_version = "{:7.7}".format(version)

        now = datetime.now()

        self.print('Setting version to {}'.format(netsol_version))

        release = "DISTRIB_ID=MVSCE\nDISTRIB_RELEASE={version}\nDISTRIB_CODENAME=\"{codename}\"\nDISTRIB_DESCRIPTION=\"{desc}\"\nDISTRIB_DATE=\"{date}\"\n".format(
                    version=version, date=now.strftime("%x"), codename=CODENAME, desc="MVSCE {}".format(version))

        temp_jcl = open("temp/sysgen05a.jcl", 'w')

        with open("jcl/sysgen05a.jcl.template", 'r') as f:
            for line in f.readlines():
                if "@VERSN@" in line:
                    if git_hash and not self.version:
                        temp_jcl.write(line.replace('@VERSN@', netsol_version))
                    else:
                        temp_jcl.write(line.replace('rev: @VERSN@', 'ver: {}'.format(netsol_version)))
                elif "@replaceme@" in line:
                    temp_jcl.write(release)
                    if git_hash:
                        temp_jcl.write("DISTRIB_HASH={}".format(git_hash))

                else:
                    temp_jcl.write(line)

        temp_jcl.close()

        self.sysgenjobs_ipl("Adding version/release info to NETSOL and SYS1.PARMLIB(RELEASE)")
        self.send_herc("devinit 12 temp/sysgen05a.jcl")
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.check_maxcc(jobname='SYSGEN5A')
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("23_SYSGEN5A")


    def sysgen06(self):
        self.set_step("step_04_system_generation","sysgen06")

        self.restore_dasd("23_SYSGEN5A")
        if self.skip_steps:
            self.print("Step 4. Performing a System Generation - Building MVS 3.8j",color=Fore.CYAN)

        self.sysgenjobs_ipl("Applying required PTFs for usermods")
        self.send_herc("devinit 12 jcl/sysgen06.jcl")
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.check_maxcc(jobname='SYSGEN06', steps_cc={"SMPAPP":"0004"})
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("24_SYSGEN06")

    def hetget_sysgen01(self,outpath='temp/'):

        try:
            hetget_query_path = subprocess.check_output(["which", "hetget"]).strip()
        except:
            raise Exception('hercules program hetget not found')


        args = [
            hetget_query_path,
            "-a", 'tape/stage1.het',
            outpath + 'sysgen01.output', '1'
            ]


        self.print("Extracting SYSGEN01 job output from tape/stage1.het to {}".format(outpath + 'sysgen01a.output'))
        subprocess.check_call(args, stdout=subprocess.DEVNULL)

    def sysgen01_extract(self, outpath="temp/"):
        logging.debug("Extracting JCL from {}sysgen01.output".format(outpath))
        self.print("Extracting JCL from {}sysgen01.output".format(outpath))
        start = ord('a') - 1
        with open(outpath+'sysgen01.output', 'r') as sysgen01_stage1:

            for line in sysgen01_stage1:
                #f.write(line)
                if 'JOB' in line and line[0:2] == "//":
                    try:
                        f.close
                    except:
                        pass
                    job = line.split()[0][2:].strip()
                    newname = 'sysgen01{}.jcl'.format(chr(start + int(job[-1])))
                    self.print('Extracting {} to {}{}'.format(job,outpath,newname))
                    logging.debug('Extracting {} to {}{}'.format(job,outpath,newname))
                    f = open("{}{}".format(outpath,newname), 'w')

                    if 'SYSGEN6' not in job:
                        line = '{},\n// TIME=1439\n/*JOBPARM  LINES=100\n//JOBCAT  DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR\n'.format(line.rstrip())

                if '//     TYPRUN=HOLD ' in line:
                    line = "// TIME=1439\n/*JOBPARM  LINES=100\n"
                elif '//STEPY' in line:
                    line = '//STEPY      EXEC PGM=IEFBR14,REGION=512K\n'
                elif 'LABEL=EXPDT=99350' in line:
                    line = line.replace('LABEL=EXPDT=99350','LABEL=EXPDT=00000')
                try:
                    f.write(line)
                except:
                    pass

        try:
            f.close
        except:
            pass


    def step_05_usermods(self,start=False):

        if not start:
            start='AY12275'

        self.set_step("step_05_usermods")

        umods_desc = {
                    "AY12275" : "corrects TCTRSZ",
                    "JLM0001" : "Prints report of SMF accounting/statistics data at the end of each job step (IEFACTRT)",
                    "JLM0002" : "Switches SMF dataset (IEFU29)",
                    "JLM0003" : "Authorized TSO Commands (IKJEFTE2)",
                    "JLM0004" : "Authorized TSO Programs (IKJEFTE8)",
                    "SLB0002" : "Adds Megabytes (M) to REGION",
                    "SYZM001" : "suppresses the inclusion of CN(00)",
                    "TIST801" : "Modifies VTAM start-up to bypass VTAMOBJ",
                    "TJES801" : "Sets subsystem ID for JES2 during JES2 initialization",
                    "TMVS804" : "Removes START from MSTRJCL",
                    "TMVS816" : "Y2K Compliance",
                    "TTSO801" : "Eliminates message IKT012D after TSO is shut down by the operator",
                    "VS49603" : "fix excessive spin loop when MVS is run under VM",
                    "WM00017" : "Adds two JES2 commands - $U and $DP.",
                    "ZP60001" : "Adds a WTO exit to start TSO after VTAM is initialized.",
                    "ZP60002" : "Modifies TSO TEST to not stop execution after encountering an invalid opcode.",
                    "ZP60003" : "Modifies IFOX00 to accept blank statements.",
                    "ZP60004" : "Modification to display action console messages in high intensity so they are more obvious.",
                    "ZP60005" : "Modifies IOS to always maintain channel and device i/o counts.",
                    "ZP60006" : "Modifies dataset deallocation to show excp counts on deallocation messages.",
                    "ZP60007" : "Modifies TSO/VTAM to provide an option to allow VTAM trace to report the data going to and from a TSO terminal",
                    "ZP60008" : "Adds EWA and WSF support for local non-SNA 3270.",
                    "ZP60009" : "Adds NOEDIT support for TPUT and TGET to TSO/VTAM.",
                    "ZP60011" : "Captures channel program CCWS in GTF SIO trace records.",
                    "ZP60012" : "Adds reporting of interrupt code in event of an ABEND in a TSO session.",
                    "ZP60013" : "Maintain an SVC event count for each SVC number.",
                    "ZP60014" : "Adds CLIST control variable and built-in function extensions.",
                    "ZP60015" : "Extends the JES2 job search for the STATUS TSO command.",
                    "ZP60016" : "Extends the JES2 job search for the STATUS TSO command.",
                    "ZP60017" : "Moves the master trace table to CSA.",
                    "ZP60018" : "Report module name/entry point if PSW address is in PLPA.",
                    "ZP60019" : "Causes recording of CPU time when TIME=1440 coded in JCL.",
                    "ZP60020" : "Removes the SYSLIN blksize limit of 3200 on the Linkage Editor.",
                    "ZP60021" : "Display keyboard characters in a printed dump.",
                    "ZP60022" : "Allow format 1 STAX parameter list to function correctly.",
                    "ZP60026" : "Adds REUSE operand to the TSO ALLOCATE command.",
                    "ZP60027" : "Adds timestamp support for Link Editor identification record.",
                    "ZP60028" : "Improves printing of the module header in printed dumps.",
                    "ZP60029" : "Customize EBCDIC <=> ASCII translate tables.",
                    "ZP60030" : "Correct MF/1 channel measurement and logging to SMF (type 73 record).",
                    "ZP60031" : "Allows type 6 and type 26 SMF records to be collected for Started Tasks.",
                    "ZP60032" : "Allows GTTERM macro to report the terminal name.",
                    "ZP60033" : "Adds support for the LOC parameter to the GETMAIN macro.",
                    "ZP60034" : "Resolve &SYSUID variable and supply USER= and PASSWORD= on JOB cards for jobs submitted through TSO.",
                    "ZP60035" : "Support any MVS-supported DASD type for LOGREC.",
                    "ZP60036" : "Support any MVS-supported DASD type for LOGREC.",
                    "ZP60037" : "Support any MVS-supported DASD type for LOGREC.",
                    "ZP60038" : "Adds CLIST variable processing Application Program Interface (IKJCT441).",
                    "ZP60039" : "Adds TEXT= operand to WTO and WTOR macros.",
                    "ZUM0007" : "Y2K Compliance",
                    "ZUM0008" : "Y2K Compliance",
                    "SYZJ2001" : "Adds condition code to job message in TSO",
                    "DYNPROC" : "Adds dynamic proclib support"
                    }

        expected_cc = {"STEP20": "0004"}


        s = usermods.index(start)
        umods = usermods[s:]
        backup_num = 1 + s

        if backup_num == 1:
            restore = "24_SYSGEN06"
        else:
            restore = "usermod_{:02}_{}".format(backup_num-1,usermods[s-1])

        self.print("Step 5. Applying Usermods",color=Fore.CYAN)
        for umod in umods:
            self.set_step("step_05_usermods",umod)
            self.restore_dasd(restore)
            self.sysgenjobs_ipl("Applying usermod {}: {}".format(umod, umods_desc[umod]))
            self.send_herc("devinit 12 usermods/{}.jcl".format(umod))
            self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
            self.check_maxcc(jobname=umod, steps_cc=expected_cc)
            self.shutdown_mvs()
            self.quit_hercules(msg=False)
            restore = "usermod_{:02}_{}".format(backup_num,umod)
            backup_num += 1
            self.backup_dasd(restore)

        self.backup_dasd("25_USERMODS")

        self.print("Applying Usermods Complete",color=Fore.GREEN)


    def step_06_fdz1d02(self):
        self.set_step("step_06_fdz1d02")
        self.restore_dasd("25_USERMODS")
        self.print("Step 6. Installing Release 13.0 of the Device Support Facilities (ICKDSF)",color=Fore.CYAN)
        self.sysgenjobs_ipl("Submitting fdz1d02")
        self.send_herc("devinit 170 tape/dz1d02.het")
        self.send_herc("devinit 12 jcl/fdz1d02.jcl")
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.check_maxcc(jobname='FDZ1D02')
        self.shutdown_mvs()
        self.quit_hercules(msg=False)
        self.backup_dasd("26_FDZ1D02")
        self.print("Installing ICKDSF 13.0 Complete",color=Fore.GREEN)

    def step_07_customization(self, start=False):
        self.set_step("step_07_customization")
        '''macro function to run the various steps
            The start variable allows you to skip to specific steps'''
        logging.debug("step_07_customization: starting at step {}".format(start))

        self.skip_steps = False
        self.print("Step 7. Customizing MVS 3.8j",color=Fore.CYAN)

        if not start or start == 'mvs000':
            self.mvs000()
            start = "dynproc"

        if start == 'dynproc':
            self.dynproc()
            start = "date"

        if start == 'date':
            self.date()
            start = "add_users"

        if start == 'add_users':
            self.add_users()
            start = "brexx"

        if start == 'brexx':
            self.brexx()

        self.print("Customization Complete",color=Fore.GREEN)


    def mvs000(self):
        self.set_step("step_07_customization","mvs000")
        self.restore_dasd("26_FDZ1D02")
        if self.skip_steps:
            self.print("Step 7. Customizing MVS 3.8j",color=Fore.CYAN)

        try:
            os.remove('MVSCE/DASD/start1.3330')
            self.print('Removing no longer needed dasd MVSCE/DASD/start1.3330')
        except OSError:
            pass
        try:
            os.remove('MVSCE/DASD/spool0.3330')
            self.print('Removing no longer needed dasd MVSCE/DASD/spool0.3330')
        except OSError:
            pass


        shutil.copyfile("dasd/syscpk.3350", 'MVSCE/DASD/syscpk.3350')

        self.dasdinit('customizations')
        self.print("Initializing and Mounting New DASD Volumes")
        self.reset_hercules()
        self.set_configs('customization')
        #self.wait_for_string("0:0151 CKD")
        self.send_herc("ipl 150")
        self.wait_for_string("HHC00010A Enter input for console 0:0009")
        self.send_oper('r 0,clpa')
        self.wait_for_string("IFB010D ENTER 'IPL REASON,SUBSYSTEM ID' OR 'U'")
        self.send_reply('u')
        self.wait_for_string('$HASP426 SPECIFY OPTIONS - HASP-II, VERSION JES2 4.1')
        self.send_reply('format,noreq')
        self.wait_for_string('$HASP479 UNABLE TO OBTAIN CKPT DATA SET LOCK - IO ERROR')
        self.send_reply('y')
        self.wait_for_string('$HASP436 REPLY Y OR N TO CONFIRM CHECKPOINT RECORD CHANGE')
        self.send_reply('y')
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        dasd = [
            "180 3380 MVSCE/DASD/pub000.3380",
            "190 3390 MVSCE/DASD/pub001.3390 cu=3880",
            "220 2314 MVSCE/DASD/sortw1.2314",
            "221 2314 MVSCE/DASD/sortw2.2314",
            "222 2314 MVSCE/DASD/sortw3.2314",
            "223 2314 MVSCE/DASD/sortw4.2314",
            "224 2314 MVSCE/DASD/sortw5.2314",
            "225 2314 MVSCE/DASD/sortw6.2314",
            "253 3350 MVSCE/DASD/syscpk.3350"
        ]
        for i in dasd:
            self.send_herc('attach {}'.format(i))
        self.wait_for_string("HHC00414I 0:0253 CKD file MVSCE/DASD/syscpk.3350")
        self.submit_file('jcl/mvs00.jcl')

        dev = ["180","190","220","221","222","223","224","225"]

        for i in dev:
            self.wait_for_string("ICK003D REPLY U TO ALTER VOLUME {:0>4} CONTENTS".format(i))
            self.send_reply('u')

        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.check_maxcc("MVS00")
        self.print("Generating user catalogs")
        self.submit_file('jcl/mvs01.jcl')
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.check_maxcc("MVS01")
        self.shutdown_mvs(cust=True)
        self.quit_hercules(msg=False)
        self.backup_dasd("27_MVS00")


    def dynproc(self):
        self.set_step("step_07_customization","dynproc")
        self.restore_dasd("27_MVS00")
        if self.skip_steps:
            self.print("Step 7. Customizing MVS 3.8j",color=Fore.CYAN)

        self.custjobs_ipl("Installing Dynamic Proclib usermod")
        self.submit_file('usermods/DYNPROC.jcl')
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.check_maxcc("DYNPROC", steps_cc={"DYNACCP" : "0004"})
        self.shutdown_mvs(cust=True)
        self.quit_hercules(msg=False)
        self.backup_dasd("28_DYNPROC")

    def date(self):
        self.set_step("step_07_customization","date")
        self.restore_dasd("28_DYNPROC")
        if self.skip_steps:
            self.print("Step 7. Customizing MVS 3.8j",color=Fore.CYAN)

        self.custjobs_ipl("Installing DATE tso command")
        self.submit_file('jcl/date.jcl')
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.check_maxcc("DATE$")
        self.shutdown_mvs(cust=True)
        self.quit_hercules(msg=False)
        self.backup_dasd("29_DATE")

    def add_users(self):
        self.set_step("step_07_customization","add_users")
        self.restore_dasd("29_DATE")
        if self.skip_steps:
            self.print("Step 7. Customizing MVS 3.8j",color=Fore.CYAN)

        self.custjobs_ipl("Adding users from users.conf")
        #IBMUSER SYS1   IKJACCNT OPER   ACCT   JCL MOUNT
        jobcard = "//{userid}A JOB (1),'ADDUSER',CLASS=S,MSGLEVEL=(1,1),MSGCLASS=A\n"


        with open("users.conf", 'r') as users:
            for user in users:
                if "#" in user[0]:
                    # comment skipped
                    continue
                u = user.split()
                self.print("Adding user {}".format(u[0]))
                logging.debug("Adding user: {}".format(user))
                self.submit(USERJOB.format(
                    usern = u[0].upper(), passwd=u[1].upper(), proc = u[2],
                    oper=u[3], acct = u[4], jcl=u[5], mount=u[6]
                ))
                self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
                self.check_maxcc(u[0])

        if self.username:
            self.print("Adding user {}".format(self.username.upper()))
            logging.debug("Adding user: {}".format(self.username.upper()))
            self.submit(USERJOB.format(
                usern = self.username.upper(), passwd=self.password.upper(), proc = 'IKJACCNT',
                oper='OPER', acct = 'ACCT', jcl='JCL', mount='MOUNT'
            ))
            self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
            self.check_maxcc(self.username.upper())


        #self.submit_file('jcl/mvs02.jcl')
        #self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.shutdown_mvs(cust=True)
        self.quit_hercules(msg=False)
        self.backup_dasd("30_MVS02")


    def brexx(self):
        self.set_step("step_07_customization","brexx")
        self.restore_dasd("30_MVS02")
        if self.skip_steps:
            self.print("Step 7. Customizing MVS 3.8j",color=Fore.CYAN)

        self.custjobs_ipl("Installing BREXX/370", clpa=True)
        self.send_herc("devinit 01c xmi/brexx.xmi")
        self.submit_file('jcl/brexx.jcl')
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.check_maxcc("BRXXINST")
        self.shutdown_mvs(cust=True)
        self.quit_hercules(msg=False)
        self.backup_dasd("31_BREXX")

    def step_08_rakf(self):
        self.set_step("step_08_rakf")
        self.restore_dasd("31_BREXX")

        self.skip_steps = False
        self.print("Step 8. Installing RAKF",color=Fore.CYAN)
        user = "MVS-sysgen"
        repo = "RAKF"
        self.custjobs_ipl("Getting current RAKF release from github", clpa=True)
        self.git_clone("https://github.com/mainframed/RAKF")
        self.print("Generating temp/RAKF/rakf_install.jcl")
        with open("temp/rakf_users.txt", 'w') as out_rakf:
            rakf_line = "{user:7.7}  {group:8.8}{groups:1}{password:8.8} {oper:1}\n"
            with open(self.users, 'r') as mvsusers:
                for line in mvsusers.readlines():
                    if "#" in line[0]:
                        # comment skipped
                        continue
                    l = line.strip().split()
                    username = l[0].upper()
                    password = l[1].upper()
                    oper = 'N'
                    groups = ' '
                    admin = False
                    group = "USER"
                    if l[3].upper() == 'OPER':
                        oper = "Y"
                    if  l[7].upper() == 'ADMIN':
                        admin = True
                        groups = "*"
                        group = "ADMIN"

                    out_rakf.write(rakf_line.format(user=username,password=password,group=group,groups=groups,oper=oper))
                    if admin:
                        out_rakf.write(rakf_line.format(user=username,password=password,group='RAKFADM',groups=groups,oper=oper))
            if self.username:
                out_rakf.write(rakf_line.format(user=self.username.upper(),password=self.password.upper(),group="ADMIN",groups="*",oper="Y"))
                out_rakf.write(rakf_line.format(user=self.username.upper(),password=self.password.upper(),group="RAKFADM",groups="*",oper="Y"))

        self.submit(subprocess.check_output(['temp/RAKF/generate_release.py','-u',Path('temp/rakf_users.txt').resolve(),'-p',self.profiles]).decode())
        self.print("Installing RAKF Release")
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.check_maxcc("RAKFINST", steps_cc={'DELETE' : '0008', 'APPLYR' : '0004',  'ZJW00032' : '0004'})
        self.custjobs_ipl("Resetting MVSCE with CLPA to complete install", clpa=True)
        self.shutdown_mvs(cust=True)
        self.quit_hercules(msg=False)
        self.backup_dasd("32_RAKF")

    def custjobs_ipl(self, step_text='', clpa=False):
        self.print(step_text)
        self.reset_hercules()
        self.set_configs('customization2')
        #self.wait_for_string("0:0151 CKD")
        self.send_herc("ipl 150")
        self.wait_for_string("HHC00010A Enter input for console 0:0009")
        if clpa:
            self.send_oper("r 0,clpa")
        else:
            self.send_oper()
        self.wait_for_string('$HASP426 SPECIFY OPTIONS - HASP-II, VERSION JES2 4.1')
        self.send_reply('noreq')
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")

    def shutdown_mvs(self, cust=False):
        self.send_oper('$p jes2')
        if cust:
            self.wait_for_string('IEF404I JES2 - ENDED - ')
        else:
            self.wait_for_string('IEF196I IEF285I   VOL SER NOS= SPOOL0.')
        self.send_oper('z eod')
        self.wait_for_string('IEE334I HALT     EOD SUCCESSFUL')
        self.send_oper('quiesce')
        self.wait_for_string("disabled wait state")
        self.send_herc('stop')

    def backup_dasd(self, step, path="backup", folder="MVSCE/DASD"):
        logging.debug("Backing up DASD, step: {}".format(step))
        # This was too slow
        # with tarfile.open('backup/{}.tar'.format(step), mode='w') as archive:
        #     archive.add('MVSCE/DASD')
        try:
            tar_query_path = subprocess.check_output(["which", "tar"]).strip()
        except:
            raise Exception('tar not found')

        outfolder = "{}.tar".format(step)
        if len(path) > 0:
            outfolder = '{}/{}.tar'.format(path,step)


        args = [
            tar_query_path, '-c', '-f',
            outfolder, folder
            ]
        subprocess.check_call(args, stdout=subprocess.DEVNULL)

    def restore_dasd(self, step):
        logging.debug("Restoring DASD, from step: {}".format(step))

        if not Path("backup/{}.tar".format(step)).is_file():
            raise ValueError("Attempting to restore from {0} but backup/{0}.tar does not exist".format(step))

        try:
            shutil.rmtree(self.path)
        except:
            pass

        # This was too slow
        # with tarfile.open('backup/{}.tar'.format(step), mode='r') as archive:
        #     archive.extractall()
        try:
            tar_query_path = subprocess.check_output(["which", "tar"]).strip()
        except:
            raise Exception('tar not found')

        args = [
            tar_query_path, '-x', '-f',
            'backup/{}.tar'.format(step)
            ]
        subprocess.check_call(args, stdout=subprocess.DEVNULL)

    def check_maxcc(self, jobname, steps_cc={}, printer_file='prt00e.txt'):
        '''Checks job and steps results, raises error
            If the step is in steps_cc, check the step vs the cc in the dictionary
            otherwise checks if step is zero
        '''
        logging.debug("Checking {} job results".format(jobname))

        found_job = False
        failed_step = False

        logmsg = '[MAXCC] Jobname: {:<8} Procname: {:<8} Stepname: {:<8} Exit Code: {:<8}'

        with open(printer_file, 'r', errors='ignore') as f:
            for line in f.readlines():
                if 'IEF142I' in line and jobname in line:

                    found_job = True

                    x = line.strip().split()
                    y = x.index('IEF142I')
                    j = x[y:]

                    log = logmsg.format(j[1],'',j[2],j[10])
                    maxcc=j[10]
                    stepname = j[2]

                    if j[3] != "-":
                        log = logmsg.format(j[1],j[2],j[3],j[11])
                        stepname = j[3]
                        maxcc=j[11]

                    logging.debug(log)

                    if stepname in steps_cc:
                        expected_cc = steps_cc[stepname]
                    else:
                        expected_cc = '0000'

                    if maxcc != expected_cc:
                        error = "Step {} Condition Code does not match expected condition code: {} vs {} review prt00e.txt for errors".format(stepname,j[-1],expected_cc)
                        logging.debug(error)
                        failed_step = True

        if not found_job:
            raise ValueError("Job {} not found in printer output {}".format(jobname, printer_file))
        if failed_step:
            raise ValueError(error)


    def reset_hercules(self):
        logging.debug('Restarting hercules')
        self.quit_hercules(msg=False)

        if self.skip_steps:
            self.skip_steps = False
        # drain STDERR and STDOUT
        while True:
            try:
                line = self.stdout_q.get(False).strip()
            except queue.Empty:
                break

        while True:
            try:
                line = self.stderr_q.get(False).strip()
            except queue.Empty:
                break

        reset_herc_event.set()

        if not self.herccmd:
            try:
                self.hercmd = subprocess.check_output(["which", "hercules"]).strip()
            except:
                raise Exception('hercules not found')

        logging.debug("Launching hercules: {}".format(self.herccmd))

        self.hercproc = subprocess.Popen([self.herccmd, '--externalgui'],
                    stdin=subprocess.PIPE,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    universal_newlines=True)
        reset_herc_event.clear()
        quit_herc_event.clear()
        self.start_threads()

        self.rc = self.hercproc.poll()
        if self.rc is not None:
            raise("Unable to start hercules")
        logging.debug("Hercules launched")
        self.set_configs('generic')
        #self.write_logs()
        logging.debug("Hercules Re-initialization Complete")


    def quit_hercules(self, msg=True):
        if msg:
            self.print("Shutting down hercules")
        if not self.hercproc or self.hercproc.poll() is not None:
            logging.debug("Hercules already shutdown")
            return
        quit_herc_event.set()
        self.send_herc('quit')
        self.wait_for_string('Hercules shutdown complete', stderr=True)
        if msg:
            self.print('Hercules has exited')

    def wait_for_string(self, string_to_waitfor, stderr=False, timeout=False):
        '''
           Reads stdout queue waiting for expected response, default is
           to check STDOUT queue, set stderr=True to check stderr queue instead
           default timeout is 30 minutes
        '''
        time_started = time.time()

        if not timeout:
            timeout = 1800

        if not timeout and self.timeout:
            timeout=self.timeout

        logging.debug("Waiting for string to appear in hercules log: {}".format(string_to_waitfor))

        while True:
            if time.time() > time_started + timeout:
                if self.substep:
                    exception = "Step: {} Substep: {} took too long".format(self.step, self.substep)
                    log = "Step: {} Substep: {} Timeout Exceeded {} seconds".format(self.step, self.substep, timeout)
                else:
                    exception = "Step: {} Timeout".format(self.step, self.substep)
                    log = "Step: {} Timeout Exceeded {} seconds".format(self.step, self.substep, timeout)
                logging.debug(log)
                raise Exception(exception)

            try:
                if stderr:
                    line = self.stderr_q.get(False).strip()
                else:
                    line = self.stdout_q.get(False).strip()

                while string_to_waitfor not in line:
                    if stderr:
                        line = self.stderr_q.get(False).strip()
                    else:
                        line = self.stdout_q.get(False).strip()
                    continue
                return

            except queue.Empty:
                continue

    def wait_for_psw(self, psw_wait, timeout=False):
        '''Reads stdout queue waiting for expected response default timeout is 30 minutes'''
        logging.debug("Waiting for PSW to end with: {}".format(psw_wait))

        time_started = time.time()

        if not timeout:
            timeout = 1800

        if not timeout and self.timeout:
            timeout=self.timeout

        while True:

            if time.time() > time_started + timeout:
                if self.substep:
                    exception = "Step: {} Substep: {} took too long".format(self.step, self.substep)
                    log = "Step: {} Substep: {} Timeout Exceeded {} seconds".format(self.step, self.substep, timeout)
                else:
                    exception = "Step: {} Timeout".format(self.step, self.substep)
                    log = "Step: {} Timeout Exceeded {} seconds".format(self.step, self.substep, timeout)
                logging.debug(log)
                raise Exception(exception)

            try:
                psw = self.stderr_q.get(False).strip()

                while psw_wait not in psw:
                    psw = self.stderr_q.get(False).strip()
                    continue
                return

            except queue.Empty:
                continue

    def step_09_cleanup(self):
        self.print("Step 9. Finalizing and Cleaning Up", color=Fore.CYAN)

        self.finalize()

        Path(running_folder+"MVSCE/conf").mkdir(parents=True, exist_ok=True)
        shutil.copy(Path('conf/local.cnf').resolve(),Path(running_folder+"MVSCE/conf").resolve())
        shutil.copy(Path('conf/mvsce.rc').resolve(),Path(running_folder+"MVSCE/conf").resolve())
        Path(running_folder+"MVSCE/printers").mkdir(parents=True, exist_ok=True)
        Path(running_folder+"MVSCE/punchcards").mkdir(parents=True, exist_ok=True)

        self.set_step(False,False)
        os.remove(".step")
        self.print("Removing hercules output files")
        for f in ["mvslog.txt","pch00d.txt","pch013.txt","pch01d.txt","prt00e.txt","prt00f.txt"]:
            if os.path.exists(f):
                os.remove(f)

        if not self.keepbackup:
            self.print("Removing backup folder")
            shutil.rmtree(Path("backup").resolve())

        if not self.keeptemp:
            self.print("Removing temp folder")
            shutil.rmtree(Path("temp").resolve())

        with open(running_folder+"MVSCE/start_mvs.sh", 'w') as script:
            script.write("hercules -f conf/local.cnf -r conf/mvsce.rc")

        self.print("Cleanup Complete",color=Fore.GREEN)

        self.backup_dasd("MVSCE.backup",path="",folder="MVSCE")

    def finalize(self):
        self.restore_dasd("32_RAKF")
        self.custjobs_ipl("Customizing SYS1.PARMLIB(COMMND00)", clpa=True)
        self.submit_file('jcl/finalize.jcl')
        self.wait_for_string("$HASP099 ALL AVAILABLE FUNCTIONS COMPLETE")
        self.check_maxcc("FINALIZE")
        self.shutdown_mvs(cust=True)
        self.quit_hercules(msg=False)
        self.backup_dasd("33_FINAL")


    def dasdinit(self, dasd_to_create):
        '''Creates empty dasd, argument is one of starter, distribution_libs, sysgen, user'''

        if dasd_to_create not in ['starter','distribution_libs','sysgen','customizations']:
            raise ValueError('Invalid DASD type {} passed to dasd init, allowable options: starter, dlibs, sysgen, or user'.format(dasd_to_create))

        try:
            dasdinit_query_path = subprocess.check_output(["which", "dasdinit"]).strip()
        except:
            raise Exception('hercules program dasdinit not found')
        folder = running_folder+"MVSCE/DASD/{}"

        dasd_dict = {
            'starter' : {
            "start1.3330" : "3330", #111111
            "spool0.3330" : "3330", #222222
            },
            'distribution_libs' : {
            "work00.3350" : "3350", #111111
            "work01.3350" : "3350", #222222
            "smp000.3350" : "3350", #333333
            },
            'sysgen' : {
            "mvsres.3350" : "3350", #111111
            "mvs000.3350" : "3350", #222222
            "spool1.3350" : "3350", #333333
            "page00.3350" : "3350", #444444
            },
            'customizations' : {
            "pub000.3380" : "3380", #111111
            "pub001.3390" : "3390", #222222
            "sortw1.2314" : "2314", #333333
            "sortw2.2314" : "2314", #444444
            "sortw3.2314" : "2314", #555555
            "sortw4.2314" : "2314", #666666
            "sortw5.2314" : "2314", #777777
            "sortw6.2314" : "2314", #888888
            }
        }

        VOLUME = 1

        for disk in dasd_dict[dasd_to_create]:

            try:
                os.remove(folder.format(disk))
                #self.print('Removing existing dasd {}'.format(folder.format(disk)))
            except OSError:
                pass

            compress = '-z'
            if self.no_compress:
                compress = ''

            dasd_type = dasd_dict[dasd_to_create][disk]
            self.print("Creating {} ({}) [{}]".format(disk, dasd_type, folder.format(disk)))
            args = [
                dasdinit_query_path,
                "-a", compress,
                folder.format(disk),
                dasd_type,
                str(VOLUME) * 6
                ]

            subprocess.check_call(args, stdout=subprocess.DEVNULL)
            VOLUME += 1

    def git_clone(self, repo):
        try:
            git_query_path = subprocess.check_output(["which", "git"]).strip()
        except:
            raise Exception('git not found')
        folder = running_folder+"temp/{}"

        args = [
            git_query_path,
            'clone',
            repo,
            folder.format(repo.split("/")[-1])
        ]

        rc = subprocess.call(args, stderr=subprocess.DEVNULL, stdout=subprocess.DEVNULL)

    def submit_file(self, jclfile, host='127.0.0.1',port=3505):
        logging.debug("[SUBMIT] Submitting {}".format(jclfile))
        with open(jclfile, 'r') as f:
            jcl = f.read()
            self.submit(jcl)

    def submit(self,jcl, host='127.0.0.1',port=3505):
        '''submits a job (in ASCII) to hercules listener'''

        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        try:
            # Connect to server and send data
            sock.connect((host, port))
            sock.send(jcl.encode())
        finally:
            sock.close()


def main():


    steps = {
        'step_01_build_starter'     : False,
        'step_02_install_smp4'      : False,
        'step_03_build_dlibs'       : [ 'smpmount',
                                        'smpjob00',
                                        'smpjob01',
                                        'smpjob02',
                                        'smpjob03',
                                        'smpjob04',
                                        'smpjob06',
                                        'smpjob07'
                                      ],
        'step_04_system_generation' : [
                                        "sysgen00",
                                        "sysgen01",
                                        "sysgen01a",
                                        "sysgen01b",
                                        "sysgen01c",
                                        "sysgen01d",
                                        "sysgen01e",
                                        "sysgen01f",
                                        "sysgen02",
                                        "sysgen03",
                                        "sysgen04",
                                        "sysgen05",
                                        "sysgen05a",
                                        "sysgen06"
                                      ],
        'step_05_usermods'          : usermods,
        'step_07_customization'     : [
                                        "mvs000",
                                        "dynproc",
                                        "date",
                                        "add_users",
                                        "brexx"
                                      ],
        'step_06_fdz1d02'           : False,
        'step_08_rakf'              : False,
        'step_09_cleanup'           : False
    }

    main_steps = []

    all_substeps = []

    for k in steps:
        main_steps.append(k)
        all_substeps.append(steps[k])

    desc = "MVS/CE sysgen is a python script, a collection of hercules tools, JCL, source code and other tools to create and install MVS/CE."

    arg_parser = argparse.ArgumentParser(description=desc,
                        usage='%(prog)s [options]',
                        epilog="Based on the Jay Moseley sysgen",
                        formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    arg_parser.add_argument("--nobrexx", help="Do not install brexx (this will also prevent RAKF from installing)", action="store_true")
    arg_parser.add_argument("--norakf", help="Do not install RAKF", action="store_true")
    arg_parser.add_argument('-l', "--list", help="List all the steps and substeps", action="store_true")
    arg_parser.add_argument('--step', help="Restart sysgen from this step. The install will continue from here. Use --list to get a list of all steps/substeps.", choices=main_steps, default=False)
    arg_parser.add_argument('--substep', help="Restart sysgen from a steps substep. The install will continue from here. Use --list to get a list of all steps/substeps.")
    arg_parser.add_argument("--timeout", help="How long to wait for specific steps to finish in seconds before failing", default=1800)
    arg_parser.add_argument("--hercules", help="Hercules binary", default='hercules')
    arg_parser.add_argument("--no-compress", help="Do not compress DASD images", default=False, action="store_true")
    arg_parser.add_argument("--version", help="Specify a version string instead of using git version", default=False)
    arg_parser.add_argument("--username", help="Add an admin user (edit users.conf to customize all users)", default=False)
    arg_parser.add_argument("--password", help="Set the password for the added user", default="CUL8TR")
    arg_parser.add_argument("--users", help="Use a custom users file instead of users.conf", default="users.conf")
    arg_parser.add_argument("--profiles", help="User a custom RAKF profile file instead of profiles.conf", default="profiles.conf")
    arg_parser.add_argument("--keep-backup", help="Keep backup folder when sysgen completes", action="store_true")
    arg_parser.add_argument("--keep-temp", help="Keep temp folder when sysgen completes", action="store_true")
    arg_parser.add_argument('-C',"--CONTINUE", help="Restart sysgen from where it failed previously. You must pass previous arguments and append -C to continue", action="store_true")

    args = arg_parser.parse_args()

    if args.list:
        print("Listing sysgen steps and substeps")
        for k in main_steps:
            print("\nStep: {}".format(k))
            if steps[k]:
                for sub in steps[k]:
                    print("\tSubstep: {}".format(sub))
        sys.exit()

    if args.step and steps[args.step] and args.substep not in steps[args.step]:
        arg_parser.error("Substep '{}' not a valid substep name for '{}'. Use --list to see steps and substeps.".format(args.substep, args.step))

    if args.substep and not args.step:
        arg_parser.error("Cannot use --substep without --step")

    if args.username and len(args.username) > 7:
        arg_parser.error("Username {} too long. Max username length is 7.".format(args.username))
    if args.password and len(args.password) > 8:
        arg_parser.error("Password {} too long. Max password length is 8.".format(args.password))

    if not os.path.exists(args.users):
        arg_parser.error("Users file {} does not exist".format(args.users))

    if not os.path.exists(args.profiles):
        arg_parser.error("Users file {} does not exist".format(args.profiles))

    users = Path(args.users).resolve()
    profiles = Path(args.profiles).resolve()

    step = args.step
    substep = args.substep

    if args.CONTINUE:

        if args.step or args.substep:
            print("WARNING - Continue mode, skipping --step and --substep")

        if not os.path.exists(".step"):
            arg_parser.error("Attempted to continue but .step file missing.")

        with open(".step", 'r') as s:
            step_file = s.readline()
            step_file = step_file.strip().split()
            step = step_file[0]
            if len(step_file) == 2:
                substep = step_file[1]
            else:
                substep = False

    mvsce = sysgen(
                 hercbin=args.hercules,
                 version=args.version,
                 username=args.username,
                 password=args.password,
                 timeout=args.timeout,
                 users=users,
                 profiles=profiles,
                 keeptemp=args.keep_temp,
                 keepbackup=args.keep_backup,
                 no_compress=args.no_compress
                 )

    mvsce.install(step, substep)




# Fore: BLACK, RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN, WHITE, RESET.
# Back: BLACK, RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN, WHITE, RESET.
# Style: DIM, NORMAL, BRIGHT, RESET_ALL

main()

#h.kill()