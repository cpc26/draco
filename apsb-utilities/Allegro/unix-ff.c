/*
 * File name: unix-ff.c
 * Creator: Drew J. Asson, APSB
 * Created: 13 May 1993
 * Last Update: 27 July 1993
 * Purpose: Foreign function object for common lisp to take advantage
 *          of unix library commands.
 * Update Info
 * ===========
 * wait (2v) commands
 *   Returns info to why the process finished.  This includes:
 *     . Did it finish?
 *     . Did it stop because of a signal?
 *     . Did it stop because of a call to exit?
 *   Values return will be: 
 *     0  for failure of waiting on that job.
 *    -1  for successful completion of job or if pid does not exist.
 *    >0  signal or error number generated. Job failed.
 *
 */

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/wait.h>

/*
 * Dummy references to get addresses
 */

extern char *	crypt(/* char *key, char *salt */);
extern int	lockf(/* int fd, int cmd, long size */);

void dummy()
{
  static int		d_int = 0;
  static char *		d_charp = (char *) 0;
  static long		d_long = 0L;

  close(d_int);
  crypt(d_charp, d_charp);
  lockf(d_int, d_int, d_long);
  open(d_charp, d_int, d_int);
  tempnam(d_charp, d_charp);
  umask(d_int);
}

/*
 * Analyize returned status information from successful call to one
 * of the wait (2V) commands.  See update info for 29 Apr 93 for 
 *description of value returned from this function.
 */

int Decifer_Status_Info (status)
int status;
{
  register int		stop_reason;
  
  if (WIFSTOPPED(status))
    stop_reason = WSTOPSIG(status);
  else if (WIFSIGNALED(status))
    stop_reason = WTERMSIG(status);
  else if (WIFEXITED(status))
    stop_reason = WEXITSTATUS(status);

  return stop_reason;
}

/*
 * Provide a blocking wait with process id allowed to be specified.
 * The Allegro CL call (sys:os-wait) blocks, but selects a process
 * at random, from processes waiting to complete.  The actual order
 * in Allegro 4.2.beta seems to be stack-based, where the latest
 * process invoked via (run-shell-command ... :wait nil) is on the top.
 */

int Blocking_Wait_With_PID (pid)
int pid;
{
  static int		statusp;
  register int		result, stat;

  result = waitpid(pid, &statusp, 0);
  if (result == pid) {
    stat = Decifer_Status_Info(statusp);
    if (stat == 0)
      return -1;
    else 
      return stat;
  }
  else return 0;

}

/*
 * Provide a non-blocking wait with process id specified.  See above
 * comment in regards to problems with Allegro CL.  This function will
 * check the status of a process and return that status.  If the function
 * is ready to die, this will clean it up.
 */

int Non_Blocking_Wait_With_PID (pid)
int pid;
{
  static struct rusage	r_use;
  static int		statusp;
  register int		stat, result;

  result = wait4(pid, (union wait *) &statusp, WNOHANG, &r_use);

  /* if error occurs, just say successful completion. */
  if (result == -1) return -1;

  if (result == pid) {
    stat = Decifer_Status_Info(statusp);
    if (stat == 0)
      return -1;
    else return stat;
  }
  else return 0;
}

/*
 * Return the value of errno.
 */

int get_errno()
{
  return errno;
}

/* Call to lock file */
int lock_file(fd, method)
int fd;
{
  return lockf(fd, method, 0);
}

/* Return mode that new files will be created with */
int Get_File_Mode ()
{
  int mask = umask(0);

  umask(mask);
  return 0666 ^ mask;
}

/*
 * Get a file lock.  If file exists, return lock if possible.  If file
 * does not exist, create it if Create_p != 0.  Returns a file descriptor
 * for resource.  If Non_Blocking != 0, then apply a non_blocking lockf
 * call.  If cannot get lock, return -1 and check errno.
 */
int Get_File_Lock(File_Name, Create_p, Non_Blocking)
char *File_Name;
int Create_p;
int Non_Blocking;
{

  int fd, lock_result, result;

  if ((fd = open(File_Name, O_RDWR | O_APPEND, Get_File_Mode())) == -1) {
    if ((errno == ENOENT) && Create_p) {
      fd = open(File_Name, O_RDWR | O_CREAT, Get_File_Mode());
      if (fd == -1) result = -1;
    }
    else result = -1;
  }

  if (fd > 0) {     /* successful open */
    
    /* obtain a lock on the file */
    if (Non_Blocking == 0)
      lock_result = lock_file (fd, F_LOCK);
    else
      lock_result = lock_file (fd, F_TLOCK);

    if (lock_result == 0)
      result = fd;
    else {
      result = -1;
      close(fd);
    }
  }

  return result;
}

/* Release a file lock, given its descriptor. */
int Release_File_Lock(fd)
int fd;
{
  int result;

  if (fd > 0) {
    lock_file (fd, F_ULOCK);
    close(fd);
    result = 0;
  }
  else
    result = -1;

  return result;
}
  
