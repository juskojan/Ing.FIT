#!/usr/bin/env python
import pigpio
import time
class DHT11:
    def __init__(self,gpio):
        self.pi             = pigpio.pi()
        self.gpio           = gpio
        self.humidity       = -100
        self.temperature    = -100
        self.high_tick      = 0
        self.bit            = 40  # how many bits we accept
        self.pi.set_pull_up_down(gpio, pigpio.PUD_OFF)
        self.pi.set_watchdog(gpio, 0)
        self.cb = self.pi.callback(gpio, pigpio.EITHER_EDGE, self._callback)

    def _callback(self, gpio, level, tick):

        # Calculate tick difference
        diff = pigpio.tickDiff(self.high_tick, tick)

        # Determine if bit 1 or 0
        if level == 0:
            if diff >= 50:
                val = 1
                if diff >= 200:
                    # Something is wrong
                    self.checksum = 256
            else:
                val = 0

            if self.bit >= 40:
                # Data accept finished
                self.bit = 40

            elif self.bit >= 32:
                # Save checksum byte
                self.checksum = (self.checksum << 1) + val

                if self.bit == 39:
                    # 40th bit received.
                    self.pi.set_watchdog(self.gpio, 0)
                    bitsum = self.hIntegral + self.hDecimal + self.tIntegral + self.tDecimal
                    if (bitsum & 255) == self.checksum:
                        # Correct checksum 
                        self.humidity = (self.hIntegral)
                        self.temperature = (self.tIntegral)
                                              

            elif self.bit >= 24:
                # Save temperature decimal data
                self.tDecimal = (self.tDecimal << 1) + val

            elif self.bit >= 16:
                # Save temperature integral data
                self.tIntegral = (self.tIntegral << 1) + val

            elif self.bit >= 8:
                # Save humidity decimal data
                self.hDecimal = (self.hDecimal << 1) + val

            elif self.bit >= 0:
                # Save humidity integral data
                self.hIntegral = (self.hIntegral << 1) + val

            self.bit += 1

        elif level == 1:
            self.high_tick = tick
            # Reset
            if diff > 250000:
                self.bit = -2
                self.hIntegral = 0
                self.hDecimal = 0
                self.tIntegral = 0
                self.tDecimal = 0
                self.checksum = 0

        else:
            # Fatal
            self.pi.set_watchdog(self.gpio, 0)

    def getTemperature(self):
        return self.temperature

    def getHumidity(self):
        return self.humidity

    def startSensor(self):
        self.pi.write(self.gpio, pigpio.LOW)
        time.sleep(0.020)  # wait at least 20ms to signal sensor to
        self.pi.set_mode(self.gpio, pigpio.INPUT)
        self.pi.set_watchdog(self.gpio, 200)

    def clean(self):
        self.pi.set_watchdog(self.gpio, 0)
        self.cb = None
        self.pi.stop()