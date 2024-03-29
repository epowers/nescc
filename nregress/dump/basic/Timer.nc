// $Id: Timer.nc,v 1.1 2005/03/25 22:34:36 idgay Exp $

/*									tab:4
 * "Copyright (c) 2000-2003 The Regents of the University  of California.  
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice, the following
 * two paragraphs and the author appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS."
 *
 * Copyright (c) 2002-2003 Intel Corporation
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached INTEL-LICENSE     
 * file. If you do not find these files, copies can be found by writing to
 * Intel Research Berkeley, 2150 Shattuck Avenue, Suite 1300, Berkeley, CA, 
 * 94704.  Attention:  Intel License Inquiry.
 */

/**
 * This interface provides a generic timer that can be used to generate
 * events at regular intervals.
 *
 * @author Su Ping
 * @author Sam Madden
 * @author David Gay
 * @modified 7/16/02
 */
includes Timer; // make TIMER_x constants available
interface Timer {

  /**
   * Start the timer. 
   * @param type The type of timer to start. Valid values include
   *  'TIMER_REPEAT' for a timer that fires repeatedly, or 
   *  'TIMER_ONE_SHOT' for a timer that fires once.
   *  @param interval The timer interval in <b>binary milliseconds</b> (1/1024
   *  second). Note that the 
   *    timer cannot support an arbitrary range of intervals.
   *    (Unfortunately this interface does not specify the valid range
   *    of timer intervals, which are specific to a platform.)
   *  @return Returns SUCCESS if the timer could be started with the 
   *    given type and interval. Returns FAIL if the type is not
   *    one of TIMER_REPEAT or TIMER_ONE_SHOT, if the timer rate is
   *    too high, or if there are too many timers currently active.
   */
  command result_t start(char type, uint32_t interval);

  /**
   * Stop the timer, preventing it from firing again.
   * If this is a TIMER_ONE_SHOT timer and it has not fired yet,
   * prevents it from firing.
   * @return SUCCESS if the timer could be stopped, or FAIL if the timer 
   * is not running or the timer ID is out of range.
   */
  command result_t stop();

  /**
   * The signal generated by the timer when it fires.
   */
  event result_t fired();
}

