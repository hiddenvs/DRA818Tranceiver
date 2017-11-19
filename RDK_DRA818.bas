
$hwstack = 128
$swstack = 128
$framesize = 128
$regfile = "m328pdef.dat"
$crystal = 1000000                                          'set fuse bits to 8 MHZ internal RC and enable Division with 8
$baud = 9600

'*******************************************************************************
$lib "glcd-Nokia3310.lib"                                   'put that lib to Bascom-AVR/Lib directory
$lib "glcd.lib"

'Config Graphlcd = 128x64sed , Rst = Portc.4 , Cs1 = Portc.3 , A0 = Portc.2 , Si = Portc.0 , Sclk = Portc.1
Config Graphlcd = 128x64sed , Rst = Portb.3 , Cs1 = Portc.3 , A0 = Portc.2 , Si = Portc.0 , Sclk = Portc.1
'Config Graphlcd = 128x64sed , Cs1 = Portc.3 , A0 = Portc.2 , Si = Portc.0 , Sclk = Portc.1
'***************************** Program *****************************************

Initlcd
Cls
Glcdcmd 33 : Glcdcmd 190                                    'Normal Contrast

$lib "i2c_twi.lbx"                                          ' we do not use software emulated I2C but the TWI
Config Scl = Portc.5                                        ' we need to provide the SCL pin name
Config Sda = Portc.4                                        ' we need to provide the SDA pin name
I2cinit                                                     ' we need to set the pins in the proper state

Const Clkwr = 208
Const Clkrd = 209

Dim Lbtxt As String * 6
Dim Lbtxt2 As String * 9

Dim Frx_e As Eram Single At 1
Dim Frx As Single                                           '
Dim Frx1 As Single                                          '
Dim Ftx As Single                                           '
Dim Ftx1 As Single                                          '

Dim Scanmin_e As Eram Single At 9
Dim Scanmin As Single

Dim Scanminchannel_e As Eram Byte At 13
Dim Scanminchannel As Byte

Dim Scanmax_e As Eram Single At 14
Dim Scanmax As Single

Dim Scanmaxchannel_e As Eram Byte At 18
Dim Scanmaxchannel As Byte

Dim Monifreq_e As Eram Single At 19
Dim Monifreq As Single

Dim Monifreqchannel_e As Eram Byte At 23
Dim Monifreqchannel As Byte

Dim Tx_ctcss_e As Eram Byte At 24
Dim Tx_ctcss As Byte
Dim Rx_ctcss_e As Eram Byte At 25
Dim Rx_ctcss As Byte
Dim Trx_ctcss As Byte

Dim Squ_level_e As Eram Byte At 26
Dim Squ_level As Byte
Dim Oldsqu_level As Byte

Dim Mod_level_e As Eram Byte At 27
Dim Mod_level As Byte

Dim Vol_level_e As Eram Byte At 28
Dim Vol_level As Byte

Dim Step_freq_e As Eram Single At 29
Dim Step_freq As Single                                     ', Freq1 As Single       ', Step_freq1 As Dword
Step_freq = 125

Dim Doscan_e As Eram Byte At 33
Dim Doscan As Byte

Dim Doreverse_e As Eram Byte At 34
Dim Doreverse As Byte

Dim Scanchannel_e As Eram Byte At 35
Dim Scanchannel As Byte
Dim Lastscanchannel As Byte

Dim Lastvhf_e As Eram Single At 36
Dim Lastvhf As Single
Dim Lastvhfscanchannel_e As Eram Byte At 40
Dim Lastvhfscanchannel As Byte
Dim Lastuhf_e As Eram Single At 5
Dim Lastuhf As Single
Dim Lastuhfscanchannel_e As Eram Byte At 41
Dim Lastuhfscanchannel As Byte

Dim Buttonmode_e As Eram Byte At 42
Dim Buttonmode As Byte

Dim Contrast_e As Eram Byte At 43
Dim Contrast As Byte

Dim I2c_channel_e As Eram Byte At 44
Dim I2c_channel As Byte

Dim Usectcss_e As Eram Byte At 45
Dim Usectcss As Byte

Dim Swapband As Bit
Dim Channel As Single
Dim Chsbyte As Word
Dim Chvbyte As Word
Dim Chround As Single
Dim I2c_enabled As Bit
Dim I2c_active As Bit
Dim I2c_commandmode As Bit
I2c_commandmode = 0

Dim Addfreq As Single
Dim Repeaterchannel As Byte

Dim Repeatercounter As Byte
Repeatercounter = 31

Dim Lowrepeater As Single
Repeaterchannel = Lookup(1 , Repeaterchannels)
Addfreq = 125 * Repeaterchannel
Lowrepeater = 1455625 + Addfreq

Dim Highrepeater As Single
Repeaterchannel = Lookup(repeatercounter , Repeaterchannels)
Addfreq = 125 * Repeaterchannel
Highrepeater = 1455625 + Addfreq

Dim Uhfrepeatercounter As Byte
Uhfrepeatercounter = 48

Dim Uhflowrepeater As Single
Repeaterchannel = Lookup(1 , Uhfrepeaterchannels)
Addfreq = 125 * Repeaterchannel
Uhflowrepeater = 4300000 + Addfreq

Dim Uhfhighrepeater As Single
Repeaterchannel = Lookup(uhfrepeatercounter , Uhfrepeaterchannels)
Addfreq = 125 * Repeaterchannel
Uhfhighrepeater = 4300000 + Addfreq

Dim Freqvar As Single
Dim Freqbyte_l As Byte
Dim Freqbyte_h As Byte

Addfreq = 0

Gosub Loadeprom
If Squ_level = 255 Then
  Gosub Getdefaults
  Gosub Saveeprom
End If

Dim Menu_nr As Byte
Menu_nr = 0

Dim Teller As Single
Teller = 0

Dim Menuteller As Single
Menuteller = 0

Dim Newfreq As Word
Newfreq = 0

Dim Savemode As Byte
Savemode = 0

Dim Scanminmode As Byte
Scanminmode = 0

Dim Scanmaxmode As Byte
Scanmaxmode = 0

Dim Scanmonimode As Byte
Scanmonimode = 0

Dim Isreverse As Byte
Isreverse = 0

Dim Startdelay As Bit
Startdelay = 0

Dim Dotune As Bit
Dotune = 0

Dim Go_on As Bit
Go_on = 0

Dim Freqhop As Bit
Freqhop = 0

Dim Blocktx As Bit
Blocktx = 0

Dim Repeatersign As String * 1
Repeatersign = " "

Dim Fromrepeater As Bit
Fromrepeater = 0

Dim Lastfreq As Single
Lastfreq = 0
Dim Lastchannel As Byte
Lastchannel = 0

Dim I2c_teller As Single
I2c_teller = 0

Dim Sdtmf As String * 4
Sdtmf = "9191"

Dim Freqstr As String * 8

Dim Secx As Byte
Dim Minx As Byte
Dim Seco As Byte
Dim Mine As Byte
Dim Hour As Byte
Dim Day As Byte
Dim Dag As Byte
Dim Maand As Byte
Dim Jaar As Byte
Dim Temp(2) As Byte
Dim Minus As Bit

Dim _seco As String * 3
Dim _mine As String * 3
Dim _hour As String * 3
Dim _day As String * 3
Dim _dag As String * 3
Dim _maand As String * 3
Dim _jaar As String * 3

Dim Hasclock As Bit
Hasclock = 1

Dim Clkteller As Byte
Clkteller = 0
Dim Tempteller As Byte
Tempteller = 0

Tx_led Alias Portd.5
Config Tx_led = Output

Rx_led Alias Portd.6
Config Rx_led = Output

Tx_pin Alias Portd.7
Config Tx_pin = Output
Tx_pin = 1

Dtmfport Alias Portb.1
Config Dtmfport = Output

Isuhf Alias Portb.5
Config Isuhf = Output
Isuhf = 0

Bipbip Alias Portb.0                                        'Beeper output
Config Bipbip = Output
Bipbip = 1                                                  'no beeping

Config Single = Scientific , Digits = 4

Dim Setradio As Bit                                         '=1 sends data on UART, =0 does not send UART
Setradio = 1                                                'at powerup send data to DRA module

Dim I2c_byte As Byte
I2c_byte = 0

Dim I2creadable As Bit
I2creadable = 0

Dim M As Byte
Dim X As Byte

Cursor Off

Gosub Biper_s
Setfont Font10x16tt
'Setfont Font8x8
Lcdat 1 , 1 , " DRA TRX"
Setfont Font5x5
Lcdat 3 , 1 , " 134MHz - 174MHz"
Lcdat 4 , 1 , " 430MHz - 440MHz"

'Lcdat 3 , 1 , "   VHF/UHF TRX"
'Lcdat 4 , 1 , "   Jutberg 2016"

Setfont Font5x5
Lcdat 6 , 1 , "(c) PA2RDK"
Lcdat 6 , 55 , "- V1.8"

For M = 175 To Contrast                                     'slow turn-on on LCD
   Glcdcmd 33 : Glcdcmd M
   Waitms 200
Next
Wait 2
Cls

Enk1 Alias Pind.3                                           'set encoder pins
Enk2 Alias Pind.2
Enk3 Alias Pind.4

Config Enk1 = Input
Config Enk2 = Input
Config Enk3 = Input
Portd.4 = 1
Portd.2 = 1
Portd.3 = 1

If Enk3 = 0 Then
  Gosub Getdefaults
  Gosub Saveeprom
  Glcdcmd 33 : Glcdcmd Contrast
  Gosub Biper_s
End If

Rx_pin Alias Pinb.2                                         'Squelch output from DRA818
Config Rx_pin = Input                                       '0=receiving a signal, 1=no signal on receive
Rx_pin = 1

Ptt Alias Pinb.4
Config Ptt = Input
Ptt = 1
Dim Dotransmit As Bit
Dotransmit = 0
Dim I2ctransmit As Bit
I2ctransmit = 0

Config Timer0 = Timer , Prescale = 1

Dim B2 As Byte
Enable Interrupts

Config Int0 = Falling
Config Int1 = Falling

Enable Int0
Enable Int1

On Int0 Enkoder_sub1
On Int1 Enkoder_sub2

'enable wake up from Powerdown with encoder or encoder button
Pcicr.2 = 1
Pcmsk2 = &B00011100

'wake-up from powerdown with PTT
Pcicr.0 = 1
Pcmsk0.4 = 1                                                'na PCINT4

Gosub Chkclock

Gosub Sendreceiveswitch

Do
  If Swapband = 1 And Frx < 4000000 Then
    Lastvhf = Frx
    Lastvhfscanchannel = Scanchannel
    Frx = Lastuhf
    Scanchannel = Lastuhfscanchannel
    Swapband = 0
    Setradio = 1
  End If
  If Swapband = 1 And Frx >= 4000000 Then
    Lastuhf = Frx
    Lastuhfscanchannel = Scanchannel
    Frx = Lastvhf
    Scanchannel = Lastvhfscanchannel
    Swapband = 0
    Setradio = 1
  End If

  If Dotransmit = 1 And Menu_nr > 0 Then
    Gosub Biper_s
    Teller = 0
    Dotune = 0
    Gosub Handlemenu
    Menu_nr = 0
    While Ptt = 0
    Wend
    Gosub Printlcd
    Waitms 25
  End If

  If Dotransmit = 1 And Doscan > 0 And Menu_nr = 0 Then
    Gosub Biper_s
    Teller = 0
    Doscan = 0
    While Ptt = 0
    Wend
    Gosub Printlcd
    Waitms 25
  End If

  If Ptt = 0 Then
    Dotransmit = 1
    I2ctransmit = 0
  Else
    If I2ctransmit = 1 Then
      Dotransmit = 1
    Else
     Dotransmit = 0
    End If
  End If

  If Dotransmit <> Tx_led And Menu_nr = 0 And Doscan = 0 Then
    Go_on = 0
    Teller = 0
    If Ptt = 1 And Tx_led = 1 Then
      If Frx >= 1445000 And Frx <= 1449625 And Freqhop = 1 Then
        Newfreq = Rnd(38)
        Newfreq = Newfreq * 125
        Newfreq = Newfreq + 5000
        Sdtmf = Str(newfreq , 4)
        Dtmfout Sdtmf , 50
        Frx = Newfreq + 1440000
        Go_on = 1
      End If
    End If
    Gosub Sendreceiveswitch
    If Go_on = 1 Then
      Gosub Dosetradio
    Else
      Gosub Printlcd
    End If
    Go_on = 0
  End If

  If Tx_led = 0 Then
    If Rx_pin = 0 Then Rx_led = 1 Else Rx_led = 0
  End If

  If I2c_enabled = 1 Then
    I2c_teller = I2c_teller + 1
    If I2c_teller = 5000 Then
      I2c_teller = 0
      I2c_active = 1
    End If
    If I2c_active > 0 Then
      If I2c_teller = 10 Then
        I2c_teller = 0
        Disable Interrupts
        Gosub Handle_i2c
        Enable Interrupts
      End If
    End If
  End If

  If Rx_led = 1 Then
    If Doscan = 1 Then
      Doscan = 0
      Gosub Printlcd
    End If

    If Doscan = 2 Then Startdelay = 1

    If Doscan = 3 And Lastfreq > 0 Then
      Teller = 0
    End If
  End If

  If Doscan > 0 And Rx_led = 0 And Menu_nr = 0 Then
    Teller = Teller + 1

    If Startdelay = 1 And Teller = 5000 Then
      Startdelay = 0
      Teller = 0
    End If

    If Startdelay = 0 And Teller = 20 And Doscan < 3 Then
      Teller = 0
      Gosub Up
      Setradio = 1
    End If

    If Doscan = 3 And Teller = 2000 And Lastfreq = 0 Then
      Lastfreq = Frx
      Lastchannel = Scanchannel
      Frx = Monifreq
      Scanchannel = Monifreqchannel
      Teller = 0
      Setradio = 1
    End If

    If Doscan = 3 And Teller = 40 And Lastfreq > 0 Then
      Frx = Lastfreq
      Scanchannel = Lastchannel
      Lastfreq = 0
      Lastchannel = 0
      Teller = 0
      Setradio = 1
    End If

  End If

  Debounce Enk3 , 0 , Menuhandler , Sub                     'enkoder button

  If Setradio = 1 Then Gosub Dosetradio

  If Menu_nr = 0 And Hasclock = 1 Then
    Incr Clkteller
    If Clkteller = 100 Then
      Gosub Handletime
      Incr Tempteller
      If Tempteller > 20 Then Gosub Handletemp
      If Tempteller = 40 Then Tempteller = 0
      Clkteller = 0
    End If
  End If
Loop

Dosetradio:
   Setradio = 0

   If Scanmin > Scanmax Then
     Scanmax = Scanmin
     Scanmaxchannel = Scanminchannel
   End If
   Ftx = Frx
   Repeaterchannel = 0

   If Scanchannel > 0 Then
     If Frx < 4300000 Then
       Repeaterchannel = Lookup(scanchannel , Repeaterchannels)
       Addfreq = 125 * Repeaterchannel
       Frx = 1455625 + Addfreq
       Ftx = Frx - 6000
     Else
       Repeaterchannel = Lookup(scanchannel , Uhfrepeaterchannels)
       Addfreq = 125 * Repeaterchannel
       Frx = 4300000 + Addfreq
       If Scanchannel = 48 Then
         Ftx = Frx + 76000
       Else
         Ftx = Frx + 16000
       End If
     End If

     If Doreverse = 1 Then
       If Frx < 4300000 Then
         Frx = Ftx
         Ftx = Frx + 6000
       Else
         Frx = Ftx
         Ftx = Frx - 16000
         If Scanchannel = 48 Then
           Ftx = Frx - 76000
         Else
           Ftx = Frx - 16000
         End If
       End If
     End If

     If Scanchannel <> Lastscanchannel Then
       If Frx < 4300000 Then
         Tx_ctcss = Lookup(scanchannel , Ctcsstxnrs)
         Rx_ctcss = Lookup(scanchannel , Ctcssrxnrs)
       Else
         Tx_ctcss = Lookup(scanchannel , Uhfctcsstxnrs)
         Rx_ctcss = Lookup(scanchannel , Uhfctcssrxnrs)
       End If
       Lastscanchannel = Scanchannel
     End If
     Fromrepeater = 1
   Else
     If Fromrepeater = 1 Then
       Fromrepeater = 0
       Tx_ctcss = 1
       Rx_ctcss = 1
       Lastscanchannel = 0
     End If
   End If

   If Usectcss < 2 Then Tx_ctcss = 1
   If Usectcss = 0 Or Usectcss = 2 Then Rx_ctcss = 1

   Frx1 = Frx / 10000
   Ftx1 = Ftx / 10000

   If Frx < 4000000 Then
      Isuhf = 0
      If Ftx < 1440000 Or Ftx > 1460000 Then Blocktx = 1 Else Blocktx = 0
   End If
   If Frx >= 4000000 Then
      Isuhf = 1
      If Ftx < 4300000 Or Ftx > 4400000 Then Blocktx = 1 Else Blocktx = 0
   End If
   Print "AT+DMOSETGROUP=" ; 0 ; "," ; Ftx1 ; "," ; Frx1 ; "," ; Lookupstr(tx_ctcss , Ctcsscodes) ; "," ; Squ_level ; "," ; Lookupstr(rx_ctcss , Ctcsscodes)       ' ; "," ; 0       ' VHF simplex
   Print "AT+DMOSETVOLUME=" ; Vol_level
   Print "AT+DMOSETMIC=" ; Mod_level ; "," ; 0 ; "," ; 0
   Gosub Printlcd
Return

Printlcd:
  Setfont Font10x16tt
  If Ftx < Frx And Scanchannel > 0 Then
    If Tx_led = 1 Then Repeatersign = "+" Else Repeatersign = "-"
  Elseif Ftx > Frx And Scanchannel > 0 Then
    If Tx_led = 1 Then Repeatersign = "-" Else Repeatersign = "+"
  Else
    Repeatersign = " "
  End If

  If Menu_nr = 0 Then Isreverse = 1 Else Isreverse = 0
  If Tx_led = 1 Then
    If Blocktx = 1 Then
      Disable Interrupts
      Lcdat 1 , 1 , "Error     " , Isreverse
      Enable Interrupts
    Else
      Freqstr = Str(ftx1 , 4)
      Disable Interrupts
      Lcdat 1 , 1 , Left(freqstr , 3) , Isreverse
      Lcdat 1 , 29 , "." , Isreverse
      Lcdat 1 , 36 , Right(freqstr , 4) ; Repeatersign ; " " , Isreverse
      Enable Interrupts
    End If
  Else
      Freqstr = Str(frx1 , 4)
      Disable Interrupts
      Lcdat 1 , 1 , Left(freqstr , 3) , Isreverse
      Lcdat 1 , 29 , "." , Isreverse
      Lcdat 1 , 36 , Right(freqstr , 4) ; Repeatersign ; " " , Isreverse
      Enable Interrupts
  End If

  Setfont Font5x5
  If Scanchannel > 0 Then
    Disable Interrupts
    If Frx < 4300000 Then
      Lcdat 3 , 1 , "     " ; Lookupstr(scanchannel , Repeaters) ; "      "
      Lcdat 4 , 1 , "   " ; Lookupstr(scanchannel , Cities)
    Else
      Lcdat 3 , 1 , "     " ; Lookupstr(scanchannel , Uhfrepeaters) ; "      "
      Lcdat 4 , 1 , "   " ; Lookupstr(scanchannel , Uhfcities)
    End If
    Enable Interrupts
  Elseif Frx >= 1450000 And Frx <= 1460000 Then
    Channel = Frx - 1450000
    Channel = Channel / 250
    Chsbyte = Channel
    Chvbyte = Chsbyte * 2
    Chround = Chsbyte
    If Chround = Channel Then
      Disable Interrupts
      Lcdat 3 , 1 , "Channel:" ; Chsbyte ; " (V" ; Chvbyte ; ") "
      Lcdat 4 , 1 , "                  "
      Enable Interrupts
    Else
    Chvbyte = Chvbyte + 1
      Disable Interrupts
      Lcdat 3 , 1 , "Channel:V" ; Chvbyte ; "        "
      Lcdat 4 , 1 , "                  "
      Enable Interrupts
    End If
  Elseif Frx >= 4300000 And Frx <= 4400000 Then
    Channel = Frx - 4300000
    Channel = Channel / 125
    Chsbyte = Channel
    Disable Interrupts
    Lcdat 3 , 1 , "Channel:" ; Chsbyte ; "         "
    Lcdat 4 , 1 , "                  "
    Enable Interrupts
  Else
    Disable Interrupts
    Lcdat 3 , 1 , "                  "
    Lcdat 4 , 1 , "                  "
    Enable Interrupts
  End If

  Setfont Font6x8
  If Doscan = 0 Then
    Lbtxt = "OFF  "
  Elseif Doscan = 1 Then
    Lbtxt = "ON   "
  Elseif Doscan = 2 Then
    Lbtxt = "AUTO "
  Else
    Lbtxt = "MONI "
  End If

  If Menu_nr = 1 Then Isreverse = 1 Else Isreverse = 0
  If Menu_nr < 2 Then
    If Menu_nr = 1 Or Hasclock = 0 Then
      Disable Interrupts
      Lcdat 5 , 1 , "Scan:" ; Lbtxt ; "        " , Isreverse
      Enable Interrupts
    End If
  End If

  If Menu_nr = 2 Then
    Disable Interrupts
    Lcdat 5 , 1 , "Band" ; "          " , 1
    Enable Interrupts
  End If

  If Menu_nr = 3 Then
    If Doreverse = 1 Then
      Lbtxt = "ON "
    Else
      Lbtxt = "OFF"
    End If
    Disable Interrupts
    Lcdat 5 , 1 , "Reverse:" ; Lbtxt ; "       " , 1
    Enable Interrupts
  End If

  If Menu_nr = 4 Then
    If Freqhop = 1 Then
      Lbtxt = "ON "
    Else
      Lbtxt = "OFF"
    End If
    Disable Interrupts
    Lcdat 5 , 1 , "Freq. hop.:" ; Lbtxt ; "       " , 1
    Enable Interrupts
  End If

  If Menu_nr = 5 Then
    If Step_freq = 125 Then
      Lbtxt = "12.5K"
    Elseif Step_freq = 250 Then
      Lbtxt = "25K  "
    Elseif Step_freq = 1000 Then
      Lbtxt = "100K "
    Else
      Lbtxt = "1MHz "
    End If
    Disable Interrupts
    Lcdat 5 , 1 , "Step:" ; Lbtxt ; "       " , 1
    Enable Interrupts
  End If

  If Menu_nr = 6 Then
    Disable Interrupts
    Lcdat 5 , 1 , "Squelch:" ; Squ_level ; "        " , 1
    Enable Interrupts
  End If

  If Menu_nr = 7 Then
    Disable Interrupts
    Lcdat 5 , 1 , "Volume:" ; Vol_level ; "         " , 1
    Enable Interrupts
  End If

  If Menu_nr = 8 Then
    Disable Interrupts
    Lcdat 5 , 1 , "Modulatie:" ; Mod_level ; "      " , 1
    Enable Interrupts
  End If

  If Menu_nr = 9 Then
    Disable Interrupts
    Lcdat 5 , 1 , "CTCSS TX:" ; Lookupstr(tx_ctcss , Ctcssfreqs) ; "    " , 1
    Enable Interrupts
  End If

  If Menu_nr = 10 Then
    Disable Interrupts
    Lcdat 5 , 1 , "CTCSS RX:" ; Lookupstr(rx_ctcss , Ctcssfreqs) ; "    " , 1
    Enable Interrupts
  End If

  If Menu_nr = 11 Then
    If Scanminmode = 0 Then
      Lbtxt = "None "
    Else
      Lbtxt = "Tune"
    End If
    Disable Interrupts
    Lcdat 5 , 1 , "Scan -:" ; Lbtxt ; "    " , 1
    Enable Interrupts
  End If

  If Menu_nr = 12 Then
    If Scanmaxmode = 0 Then
      Lbtxt = "None "
    Else
      Lbtxt = "Tune"
    End If
    Disable Interrupts
    Lcdat 5 , 1 , "Scan +:" ; Lbtxt ; "    " , 1
    Enable Interrupts
  End If

  If Menu_nr = 13 Then
    If Scanmonimode = 0 Then
      Lbtxt = "None "
    Else
      Lbtxt = "Tune"
    End If
    Disable Interrupts
    Lcdat 5 , 1 , "Monitor:" ; Lbtxt ; "    " , 1
    Enable Interrupts
  End If

  If Menu_nr = 14 Then
    If Buttonmode = 0 Then
      Lbtxt2 = "None "
    Elseif Buttonmode = 1 Then
      Lbtxt2 = "Scan on"
    Elseif Buttonmode = 2 Then
      Lbtxt2 = "Scan auto"
    Elseif Buttonmode = 3 Then
      Lbtxt2 = "Scan moni"
    Elseif Buttonmode = 4 Then
      Lbtxt2 = "Band"
    Elseif Buttonmode = 5 Then
      Lbtxt2 = "Reverse"
    Else
      Lbtxt2 = "Squelsh"
    End If
    Disable Interrupts
    Lcdat 5 , 1 , "Btn:" ; Lbtxt2 ; "        " , 1
    Enable Interrupts
  End If

  If Menu_nr = 15 Then
    Disable Interrupts
    Lcdat 5 , 1 , "I2C Chan.:" ; I2c_channel ; "      " , 1
    Enable Interrupts
  End If

  If Menu_nr = 16 Then
    Disable Interrupts
    Lcdat 5 , 1 , "Contrast:" ; Contrast ; "      " , 1
    Enable Interrupts
   Glcdcmd 33 : Glcdcmd Contrast
  End If


  If Menu_nr = 17 Then
    If Usectcss = 0 Then
      Lbtxt2 = "None "
    Elseif Usectcss = 1 Then
      Lbtxt2 = "RX   "
    Elseif Usectcss = 2 Then
      Lbtxt2 = "TX   "
    Elseif Usectcss = 3 Then
      Lbtxt2 = "RX&TX"
    End If
    Disable Interrupts
    Lcdat 5 , 1 , "CTCSS:" ; Lbtxt2 ; "  " , 1
    Enable Interrupts
  End If

  If Menu_nr = 18 Then
    Disable Interrupts
    Lcdat 5 , 1 , "  Hours:" ; Hour ; "      " , 1
    Enable Interrupts
  End If

  If Menu_nr = 19 Then
    Disable Interrupts
    Lcdat 5 , 1 , "Minutes:" ; Mine ; "      " , 1
    Enable Interrupts
  End If

  If Menu_nr = 20 Then
    Disable Interrupts
    Lcdat 5 , 1 , "Seconds:" ; Seco ; "      " , 1
    Enable Interrupts
  End If

  If Menu_nr = 21 Then
    Disable Interrupts
    Lcdat 5 , 1 , "    Day:" ; Dag ; "      " , 1
    Enable Interrupts
  End If

  If Menu_nr = 22 Then
    Disable Interrupts
    Lcdat 5 , 1 , "  Month:" ; Maand ; "      " , 1
    Enable Interrupts
  End If

  If Menu_nr = 23 Then
    Disable Interrupts
    Lcdat 5 , 1 , "   Year:" ; Jaar ; "     " , 1
    Enable Interrupts
  End If

  If Menu_nr = 24 Then
    If Savemode = 0 Then
      Lbtxt = "None "
    Elseif Savemode = 1 Then
      Lbtxt = "Save"
    Else
      Lbtxt = "Load"
    End If
    Disable Interrupts
    Lcdat 5 , 1 , "Defaults:" ; Lbtxt ; "    " , 1
    Enable Interrupts
  End If

  Setfont Font5x5
  If Ptt = 0 And Menu_nr = 0 Then Trx_ctcss = Tx_ctcss Else Trx_ctcss = Rx_ctcss
  Disable Interrupts
  Lcdat 6 , 1 , "V:" ; Vol_level ; " Sq:" ; Squ_level ; " Ct;" ; Lookupstr(trx_ctcss , Ctcssfreqs) ; " " , 1
  Enable Interrupts
Return

Enkoder_sub1:
   Disable Interrupts
   If Enk2 = 1 And Enk1 = 0 Then Gosub Down
   If Enk2 = 0 And Enk1 = 1 Then Gosub Down
   Enable Interrupts
   Setradio = 1
Return

Enkoder_sub2:
   Disable Interrupts
   If Enk1 = 1 And Enk2 = 0 Then Gosub Up
   If Enk1 = 0 And Enk2 = 1 Then Gosub Up
   Enable Interrupts
   Setradio = 1
Return

Down:
  If Menu_nr = 0 Or Dotune = 1 Then
    If Frx < 4000000 And Frx >= Lowrepeater And Frx <= Highrepeater Then
      If Scanchannel = 0 Then Scanchannel = Repeatercounter Else Scanchannel = Scanchannel - 1
      If Scanchannel = 0 Then Frx = Lowrepeater - Step_freq
    Elseif Frx >= 4000000 And Frx >= Uhflowrepeater And Frx <= Uhfhighrepeater Then
      If Scanchannel = 0 Then Scanchannel = Uhfrepeatercounter Else Scanchannel = Scanchannel - 1
      If Scanchannel = 0 Then Frx = Uhflowrepeater - Step_freq
    Else
      Frx = Frx - Step_freq
      If Frx < 1340000 Then Frx = 4700000
      If Frx < 4000000 Then
        If Frx > 1740000 Then
          Frx = 1740000
        End If
      End If
    End If
  End If

  If Menu_nr = 1 Then
    Doscan = Doscan - 1
    If Doscan = 255 Then Doscan = 3
  End If

  If Menu_nr = 2 Then
    Swapband = 1
  End If

  If Menu_nr = 3 Then
    Doreverse = Doreverse - 1
    If Doreverse = 255 Then Doreverse = 1
  End If

  If Menu_nr = 4 Then
    If Freqhop = 1 Then Freqhop = 0 Else Freqhop = 1
    If Freqhop = 1 Then Gosub Startfreqhop
  End If

  If Menu_nr = 5 Then
    Go_on = 1
    If Step_freq = 125 And Go_on = 1 Then
      Step_freq = 10000
      Go_on = 0
    Elseif Step_freq = 10000 And Go_on = 1 Then
      Step_freq = 1000
      Go_on = 0
    Elseif Step_freq = 1000 And Go_on = 1 Then
      Step_freq = 250
      Go_on = 0
    Elseif Go_on = 1 Then
      Step_freq = 125
      Go_on = 0
    End If
  End If

  If Menu_nr = 6 Then
    Squ_level = Squ_level - 1
    If Squ_level > 8 Then Squ_level = 8
  End If

  If Menu_nr = 7 Then
    Vol_level = Vol_level - 1
    If Vol_level > 8 Then Vol_level = 8
  End If

  If Menu_nr = 8 Then
    Mod_level = Mod_level - 1
    If Mod_level > 8 Then Mod_level = 8
  End If

  If Menu_nr = 9 Then
    Tx_ctcss = Tx_ctcss - 1
    If Tx_ctcss = 0 Then Tx_ctcss = 39
  End If

  If Menu_nr = 10 Then
    Rx_ctcss = Rx_ctcss - 1
    If Rx_ctcss = 0 Then Rx_ctcss = 39
  End If

  If Menu_nr = 11 And Dotune = 0 Then
    Scanminmode = Scanminmode - 1
    If Scanminmode = 255 Then Scanminmode = 1
    If Scanminmode = 1 Then
      Frx = Scanmin
      Scanchannel = Scanminchannel
      Dotune = 1
      Doscan = 0
    End If
  End If

  If Menu_nr = 12 And Dotune = 0 Then
    Scanmaxmode = Scanmaxmode - 1
    If Scanmaxmode = 255 Then Scanmaxmode = 1
    If Scanmaxmode = 1 Then
      Frx = Scanmax
      Scanchannel = Scanmaxchannel
      Dotune = 1
      Doscan = 0
    End If
  End If

  If Menu_nr = 13 And Dotune = 0 Then
    Scanmonimode = Scanmonimode - 1
    If Scanmonimode = 255 Then Scanmonimode = 1
    If Scanmonimode = 1 Then
      Frx = Monifreq
      Scanchannel = Monifreqchannel
      Dotune = 1
      Doscan = 0
    End If
  End If

  If Menu_nr = 14 Then
    Buttonmode = Buttonmode - 1                             'None, Scan on/auto/moni, band, Reverse, Squelsh,
    If Buttonmode = 255 Then Buttonmode = 6
  End If

  If Menu_nr = 15 Then
    I2c_channel = I2c_channel - 1
  End If

  If Menu_nr = 16 Then
    Contrast = Contrast - 1
  End If

  If Menu_nr = 17 Then
    Usectcss = Usectcss - 1
    If Usectcss = 255 Then Usectcss = 3
  End If

  If Menu_nr = 18 Then
    Hour = Hour - 1
    If Hour = 255 Then Hour = 23
  End If

  If Menu_nr = 19 Then
    Mine = Mine - 1
    If Mine = 255 Then Mine = 59
  End If

  If Menu_nr = 20 Then
    Seco = Seco - 1
    If Seco = 255 Then Seco = 59
  End If

  If Menu_nr = 21 Then
    Dag = Dag - 1
    If Dag = 255 Then Dag = 31
  End If

  If Menu_nr = 22 Then
    Maand = Maand - 1
    If Maand = 0 Then Maand = 12
  End If

  If Menu_nr = 23 Then
    Jaar = Jaar - 1
    If Jaar = 255 Then Jaar = 99
  End If

  If Menu_nr = 24 Then
    Savemode = Savemode - 1
    If Savemode = 255 Then Savemode = 2
  End If
Return

Up:
  If Menu_nr = 0 Or Dotune = 1 Then
    If Doscan > 0 And Doscan < 3 Then
      If Frx > Scanmax Then
        Frx = Scanmin
        Scanchannel = Scanminchannel
      End If
      If Frx < Scanmin Then
        Frx = Scanmin
        Scanchannel = Scanminchannel
      End If
    End If

    If Frx < 4000000 And Frx >= Lowrepeater And Frx <= Highrepeater Then
      Scanchannel = Scanchannel + 1
      If Scanchannel > Repeatercounter Then
        Scanchannel = 0
        Frx = Highrepeater + Step_freq
      End If
    Elseif Frx >= 4000000 And Frx >= Uhflowrepeater And Frx <= Uhfhighrepeater Then
      Scanchannel = Scanchannel + 1
      If Scanchannel > Uhfrepeatercounter Then
        Scanchannel = 0
        Frx = Uhfhighrepeater + Step_freq
      End If
    Else
      Scanchannel = 0
      Frx = Frx + Step_freq

      If Frx > 1740000 Then
        If Frx < 4000000 Then
          Frx = 4000000
        End If
      End If
      If Frx > 4700000 Then
        Frx = 1340000
      End If
    End If
  End If

  If Menu_nr = 1 Then
    Doscan = Doscan + 1
    If Doscan = 4 Then Doscan = 0
  End If

  If Menu_nr = 2 Then
    Swapband = 1
  End If

  If Menu_nr = 3 Then
    Doreverse = Doreverse + 1
    If Doreverse = 2 Then Doreverse = 0
  End If

  If Menu_nr = 4 Then
    If Freqhop = 1 Then Freqhop = 0 Else Freqhop = 1
    If Freqhop = 1 Then Gosub Startfreqhop
  End If

  If Menu_nr = 5 Then
    Go_on = 1
    If Step_freq = 125 And Go_on = 1 Then
      Step_freq = 250
      Go_on = 0
    Elseif Step_freq = 250 And Go_on = 1 Then
      Step_freq = 1000
      Go_on = 0
    Elseif Step_freq = 1000 And Go_on = 1 Then
      Step_freq = 10000
      Go_on = 0
    Elseif Go_on = 1 Then
      Step_freq = 125
      Go_on = 0
    End If
  End If

  If Menu_nr = 6 Then
    Squ_level = Squ_level + 1
    If Squ_level > 8 Then Squ_level = 0
  End If

  If Menu_nr = 7 Then
    Vol_level = Vol_level + 1
    If Vol_level > 8 Then Vol_level = 0
  End If

  If Menu_nr = 8 Then
    Mod_level = Mod_level + 1
    If Mod_level = 9 Then Mod_level = 0
  End If

  If Menu_nr = 9 Then
    Tx_ctcss = Tx_ctcss + 1
    If Tx_ctcss = 40 Then Tx_ctcss = 1
  End If

  If Menu_nr = 10 Then
    Rx_ctcss = Rx_ctcss + 1
    If Rx_ctcss = 40 Then Rx_ctcss = 1
  End If

  If Menu_nr = 11 And Dotune = 0 Then
    Scanminmode = Scanminmode + 1
    If Scanminmode = 2 Then Scanminmode = 0
    If Scanminmode = 1 Then
      Frx = Scanmin
      Scanchannel = Scanminchannel
      Dotune = 1
      Doscan = 0
    End If
  End If

  If Menu_nr = 12 And Dotune = 0 Then
    Scanmaxmode = Scanmaxmode + 1
    If Scanmaxmode = 2 Then Scanmaxmode = 0
    If Scanmaxmode = 1 Then
      Frx = Scanmax
      Scanchannel = Scanmaxchannel
      Dotune = 1
      Doscan = 0
    End If
  End If

  If Menu_nr = 13 And Dotune = 0 Then
    Scanmonimode = Scanmonimode + 1
    If Scanmonimode = 2 Then Scanmonimode = 0
    If Scanmonimode = 1 Then
      Frx = Monifreq
      Scanchannel = Monifreqchannel
      Dotune = 1
      Doscan = 0
    End If
  End If

  If Menu_nr = 14 Then
    Buttonmode = Buttonmode + 1                             'None, Scan on/auto/moni, band, Reverse, Squelsh,
    If Buttonmode = 7 Then Buttonmode = 0
  End If

  If Menu_nr = 15 Then
    I2c_channel = I2c_channel + 1
  End If

  If Menu_nr = 16 Then
    Contrast = Contrast + 1
  End If

  If Menu_nr = 17 Then
    Usectcss = Usectcss + 1
    If Usectcss = 4 Then Usectcss = 0
  End If

  If Menu_nr = 18 Then
    Hour = Hour + 1
    If Hour > 23 Then Hour = 0
  End If

  If Menu_nr = 19 Then
    Mine = Mine + 1
    If Mine > 59 Then Mine = 0
  End If

  If Menu_nr = 20 Then
    Seco = Seco + 1
    If Seco > 59 Then Seco = 0
  End If

  If Menu_nr = 21 Then
    Dag = Dag + 1
    If Dag > 31 Then Dag = 1
  End If

  If Menu_nr = 22 Then
    Maand = Maand + 1
    If Maand > 12 Then Maand = 1
  End If

  If Menu_nr = 23 Then
    Jaar = Jaar + 1
    If Jaar > 99 Then Jaar = 1
  End If

  If Menu_nr = 24 Then
    Savemode = Savemode + 1
    If Savemode = 3 Then Savemode = 0
  End If
Return

Sendreceiveswitch:
  If Dotransmit = 1 Then
    If Blocktx = 0 Then Tx_pin = 0 Else Tx_pin = 1
    Tx_led = 1
    Rx_led = 0
  Else
    Tx_pin = 1
    Rx_led = 0
    Tx_led = 0
  End If
Return

Handlemenu:
   If Menu_nr = 11 And Scanminmode = 1 Then
     Scanmin = Frx
     Scanminchannel = Scanchannel
     Scanminmode = 0
     Dotune = 0
   End If
   If Menu_nr = 12 And Scanmaxmode = 1 Then
     Scanmax = Frx
     Scanmaxchannel = Scanchannel
     Scanmaxmode = 0
     Dotune = 0
   End If
   If Menu_nr = 13 And Scanmonimode = 1 Then
     Monifreq = Frx
     Monifreqchannel = Scanchannel
     Scanmonimode = 0
     Dotune = 0
   End If
   If Menu_nr = 15 Then
     If I2c_channel > 0 Then I2c_enabled = 1 Else I2c_enabled = 0
     I2c_active = I2c_enabled
   End If
   If Menu_nr = 18 Then
     I2cstart
     I2cwbyte Clkwr
     I2cwbyte 2
     I2cwbyte Makebcd(hour)
     I2cstop
   End If
   If Menu_nr = 19 Then
     I2cstart
     I2cwbyte Clkwr
     I2cwbyte 1
     I2cwbyte Makebcd(mine)
     I2cstop
   End If
   If Menu_nr = 20 Then
     I2cstart
     I2cwbyte Clkwr
     I2cwbyte 0
     I2cwbyte Makebcd(seco)
     I2cstop
   End If
   If Menu_nr = 21 Then
     I2cstart
     I2cwbyte Clkwr
     I2cwbyte 4
     I2cwbyte Makebcd(dag)
     I2cstop
   End If
   If Menu_nr = 22 Then
     I2cstart
     I2cwbyte Clkwr
     I2cwbyte 5
     I2cwbyte Makebcd(maand)
     I2cstop
   End If
   If Menu_nr = 23 Then
     I2cstart
     I2cwbyte Clkwr
     I2cwbyte 6
     I2cwbyte Makebcd(jaar)
     I2cstop
   End If

   If Menu_nr = 24 Then
     If Savemode = 1 Then
       Gosub Saveeprom
       Savemode = 0
     End If
     If Savemode = 2 Then
       Gosub Getdefaults
       Gosub Saveeprom
       Savemode = 0
     End If
   End If
Return

Menuhandler:
   Gosub Biper_s
   Menuteller = 0
   While Enk3 = 0 And Menuteller < 3010
    Menuteller = Menuteller + 1
   Wend
   If Menuteller > 3000 Then
     Gosub Biper_s
     If Menu_nr = 0 Then
       If Buttonmode = 1 Then
         If Doscan = 0 Then Doscan = 1 Else Doscan = 0
       End If
       If Buttonmode = 2 Then
         If Doscan = 0 Then Doscan = 2 Else Doscan = 0
       End If
       If Buttonmode = 3 Then
         If Doscan = 0 Then Doscan = 3 Else Doscan = 0
       End If
       If Buttonmode = 4 Then Swapband = 1
       If Buttonmode = 5 Then
         If Doreverse = 0 Then Doreverse = 1 Else Doreverse = 0
       End If
       If Buttonmode = 6 Then
         If Squ_level = 0 Then
           Squ_level = Oldsqu_level
         Else
           Oldsqu_level = Squ_level
           Squ_level = 0
         End If
       End If
     End If

     If Menu_nr > 0 Then
       Teller = 0
       Dotune = 0
       Gosub Handlemenu
       Menu_nr = 0
       Gosub Printlcd
       Waitms 25
     End If

     Teller = 0
     Setradio = 1
   Else
     Gosub Handlemenu
     Incr Menu_nr
     If Menu_nr = 4 Then Incr Menu_nr                       'skip frequency hopping
     'If Menu_nr = 15 Then Incr Menu_nr                      'skip i2c
     If Menu_nr = 25 Then Menu_nr = 0
     If Menu_nr = 18 And Hasclock = 0 Then Menu_nr = 24
     Teller = 0
     Setradio = 1
   End If
Return

Biper_s:
   Bipbip = 0                                               'Beeper on Portd.0
   Waitms 70
   Bipbip = 1
   Waitms 70
Return

Startfreqhop:
  If Frx < 1445000 Or Frx > 1449625 Then
    Frx = 1445250
    Scanchannel = 0
  End If
Return

Sendi2c_byte:
  I2cstart
  I2cwbyte I2c_channel
  I2cwbyte I2c_byte
  I2cstop
Return

Readi2c_byte:
  I2cstart
  I2cwbyte I2c_channel + 1
  If Err = 0 Then
    I2crbyte I2c_byte , Nack
  Else
    I2c_active = 0
  End If
  I2cstop
  If I2c_byte > 0 And I2c_active > 0 Then
    Lcdat 4 , 1 , "  I2C:" ; I2c_byte ; "        "
  End If
Return

Chkclock:
    Hasclock = 0
    I2cstart
    I2cwbyte Clkwr
    If Err = 0 Then Hasclock = 1
    I2cstop
Return

Handletime:
  I2cstart                                                  'Generate start code
  I2cwbyte Clkwr                                            'Send Address
  I2cwbyte 0
  I2cstop
  I2cstart                                                  'Generate Start Code
  I2cwbyte Clkrd                                            'Send Address
  I2crbyte Seco , Ack                                       'sec
  I2crbyte Mine , Ack                                       'minutes
  I2crbyte Hour , Ack                                       'Hours
  I2crbyte Day , Ack
  I2crbyte Dag , Ack
  I2crbyte Maand , Ack
  I2crbyte Jaar , Nack
  I2cstop
  Seco = Makedec(seco)
  Mine = Makedec(mine)
  Hour = Makedec(hour)
  Day = Makedec(day)
  Dag = Makedec(dag)
  Maand = Makedec(maand)
  Jaar = Makedec(jaar)

  If Seco > 59 Then Seco = 0
  If Mine > 59 Then Mine = 0
  If Hour > 23 Then Hour = 0
  If Day > 7 Then Day = 0
  If Dag > 31 Then Dag = 0
  If Maand > 12 Then Maand = 0
  If Jaar > 99 Then Jaar = 0

  _seco = "00" + Str(seco)
  _seco = Right(_seco , 2)
  _mine = "00" + Str(mine)
  _mine = Right(_mine , 2)
  _hour = "00" + Str(hour)
  _hour = Right(_hour , 2)
  _day = "00" + Str(day)
  _day = Right(_day , 2)
  _dag = "00" + Str(dag)
  _dag = Right(_dag , 2)
  _maand = "00" + Str(maand)
  _maand = Right(_maand , 2)
  _jaar = "00" + Str(jaar)
  _jaar = Right(_jaar , 2)

  Disable Interrupts
  Lcdat 5 , 1 , _hour ; ":" ; _mine ; ":" ; _seco ;
  Lcdat 5 , 44 , _dag ; "/" ; _maand ; "/" ; _jaar ; "     "
  Enable Interrupts
Return

Handletemp:
  I2cstart
  I2cwbyte Clkwr
  I2cwbyte &H11
  I2cstop
  I2cstart
  I2cwbyte Clkrd
  I2crbyte Temp(1) , Ack
  I2crbyte Temp(2) , Nack
  I2cstop

  Minus = Temp(1).7
  If Minus = 1 Then
      Temp(1) = Not Temp(1)
      Incr Temp(1)
  End If

  Shift Temp(2) , Right , 6
  Temp(2) = Temp(2) * 25
  Disable Interrupts
  Lcdat 5 , 44 , "  " ; Temp(1) ; "." ; Temp(2) ; "*    "
  Enable Interrupts
Return


'10/40 = Freq + (1 = confirmed)
'11/41 = Freq - (1 = confirmed)
'12/42 = Get/Set Scanmode (0=off,1=on,2=auto,3=monitor)
'13/43 = Band V/U (1=VHF, 2=UHF)
'14/44 = Reverse 0/1
'15/45 = Freq. hopping
'16/46 = Stepsize 1=12.5,2=25,3=100,4=1M
'17/47 = Squelsh level 0-8
'18/48 = Volume 0-8
'19/49 = Modulatie 0-8
'20/50 = contrast 0-255
'21/51 = CTCSS TX
'22/52 = CTCSS RX
'23/53 = Use CTCSS (0=off,1=RX,2=TX,3=RX&TX)
'24/54 = Buttonmode (0=None,1=Scan on,2=Scan auto,3=Scan moni,4=Band=5=Reverse,6=Squelsh)
'25/55 = Actual frequency(=(((byte1*256)+byte2)*125)+1340000)
'26/56 = Min. scanfrequency(=(((byte1*256)+byte2)*125)+1340000)
'27/57 = Max. scanfrequency(=(((byte1*256)+byte2)*125)+1340000)
'28/58 = Make monitor frequency actual frequency
'29/59 = Repeaterchannel
'30/60 = Get led status (0=none, 1=RX, 2=TX, 3 = both)
'31/61 = Load defaults/Save defaults
'32/62 = None/Set the actual frequency as monitor frequency
'33/63 = Get all data
'34/64 = Get/Set PTT

'Getsettings:scanmode, freq hopping, stepsize, squelsh, volume, modulation, contrast, ctcssmode, button mode, min. scanfreq,
' max scanfreq, monitor freq, frequency, led status, band, reverse, ctcss TX, ctcss RX, repeater channel

Handle_i2c:
'  Gosub Readi2c_byte
'  If I2c_byte < 40 Then
'    I2c_commandmode = 0
'  Else
'    I2c_commandmode = 1
'    I2c_byte = I2c_byte - 30
'  End If

'  If I2c_byte = 33 Then
'    I2cstart
'    I2cwbyte I2c_channel

'    Waitms 2
'    I2cwbyte Doscan

'    I2c_byte = Freqhop
'    Waitms 2
'    I2cwbyte I2c_byte

'    If Step_freq = 125 Then I2c_byte = 1
'    If Step_freq = 250 Then I2c_byte = 2
'    If Step_freq = 1000 Then I2c_byte = 3
'    If Step_freq = 10000 Then I2c_byte = 4
'    Waitms 2
'    I2cwbyte I2c_byte

'    Waitms 2
'    I2cwbyte Squ_level

'    Waitms 2
'    I2cwbyte Vol_level

'    Waitms 2
'    I2cwbyte Mod_level

'    I2c_byte = Contrast - 128
'    Waitms 2
'    I2cwbyte I2c_byte

'    Waitms 2
'    I2cwbyte Usectcss

'    Waitms 2
'    I2cwbyte Buttonmode

'    Freqvar = Scanmin - 1340000
'    Freqvar = Freqvar / 125
'    Freqbyte_l = Freqvar
'    Shift Freqvar , Right , 8
'    Freqbyte_h = Freqvar
'    I2c_byte = Freqbyte_h
'    Waitms 2
'    I2cwbyte I2c_byte
'    I2c_byte = Freqbyte_l
'    Waitms 2
'    I2cwbyte I2c_byte

'    Freqvar = Scanmax - 1340000
'    Freqvar = Freqvar / 125
'    Freqbyte_l = Freqvar
'    Shift Freqvar , Right , 8
'    Freqbyte_h = Freqvar
'    I2c_byte = Freqbyte_h
'    Waitms 2
'    I2cwbyte I2c_byte
'    I2c_byte = Freqbyte_l
'    Waitms 2
'    I2cwbyte I2c_byte

'    Freqvar = Monifreq - 1340000
'    Freqvar = Freqvar / 125
'    Freqbyte_l = Freqvar
'    Shift Freqvar , Right , 8
'    Freqbyte_h = Freqvar
'    I2c_byte = Freqbyte_h
'    Waitms 2
'    I2cwbyte I2c_byte
'    I2c_byte = Freqbyte_l
'    Waitms 2
'    I2cwbyte I2c_byte

'    Freqvar = Frx - 1340000
'    Freqvar = Freqvar / 125
'    Freqbyte_l = Freqvar
'    Shift Freqvar , Right , 8
'    Freqbyte_h = Freqvar
'    I2c_byte = Freqbyte_h
'    Waitms 2
'    I2cwbyte I2c_byte
'    I2c_byte = Freqbyte_l
'    Waitms 2
'    I2cwbyte I2c_byte

'    I2c_byte = 0
'    If Rx_led = 1 Then I2c_byte = I2c_byte + 1
'    If Tx_led = 1 Then I2c_byte = I2c_byte + 2
'    Waitms 2
'    I2cwbyte I2c_byte

'    If Frx < 4000000 Then I2c_byte = 1 Else I2c_byte = 2
'    Waitms 2
'    I2cwbyte I2c_byte

'    Waitms 2
'    I2cwbyte Doreverse

'    I2c_byte = Tx_ctcss - 1
'    Waitms 2
'    I2cwbyte I2c_byte

'    I2c_byte = Rx_ctcss - 1
'    Waitms 2
'    I2cwbyte I2c_byte

'    Waitms 2
'    I2cwbyte Scanchannel

'    Waitms 2
'    I2c_byte = Dotransmit
'    I2cwbyte I2c_byte

'    I2cstop
'  End If


'  If I2c_byte = 10 Then
'    If Menu_nr = 0 Then
'      I2c_byte = 1
'      Gosub Sendi2c_byte
'      Gosub Up
'      Setradio = 1
'    End If
'  End If

'  If I2c_byte = 11 Then
'    If Menu_nr = 0 Then
'      I2c_byte = 1
'      Gosub Sendi2c_byte
'      Gosub Down
'      Setradio = 1
'    End If
'  End If

'  If I2c_byte = 12 Then
'    I2c_byte = Doscan
'    Gosub Sendi2c_byte
'    If I2c_commandmode = 1 Then
'      Waitms 7
'      Gosub Readi2c_byte
'      Doscan = I2c_byte
'      If Doscan > 3 Then Doscan = 3
'      I2c_byte = Doscan
'      Gosub Sendi2c_byte
'      Teller = 0
'      Startdelay = 0
'      Setradio = 1
'    End If
'  End If

'  If I2c_byte = 13 Then
'    If Frx < 4000000 Then I2c_byte = 1 Else I2c_byte = 2
'    Gosub Sendi2c_byte
'    If I2c_commandmode = 1 Then
'      Waitms 7
'      Gosub Readi2c_byte
'      Gosub Sendi2c_byte
'      If I2c_byte > 2 Then I2c_byte = 2
'      If I2c_byte < 1 Then I2c_byte = 1
'      If Frx < 4000000 And I2c_byte = 2 Then
'        Lastvhf = Frx
'        Lastvhfscanchannel = Scanchannel
'        Frx = Lastuhf
'        Scanchannel = Lastuhfscanchannel
'        Setradio = 1
'      End If
'      If Frx > 4000000 And I2c_byte = 1 Then
'        Lastuhf = Frx
'        Lastuhfscanchannel = Scanchannel
'        Frx = Lastvhf
'        Scanchannel = Lastvhfscanchannel
'        Setradio = 1
'      End If
'    End If
'  End If

'  If I2c_byte = 14 Then
'    I2c_byte = Doreverse
'    Gosub Sendi2c_byte
'    If I2c_commandmode = 1 Then
'      Waitms 7
'      Gosub Readi2c_byte
'      Doreverse = I2c_byte
'      If Doreverse > 1 Then Doreverse = 1
'      Gosub Sendi2c_byte
'      Teller = 0
'      Startdelay = 0
'      Setradio = 1
'    End If
'  End If

'  If I2c_byte = 15 Then
'    I2c_byte = Freqhop
'    Gosub Sendi2c_byte
'    If I2c_commandmode = 1 Then
'      Waitms 7
'      Gosub Readi2c_byte
'      If I2c_byte = 1 Then
'         Freqhop = 1
'      Else
'         Freqhop = 0
'      End If
'      Gosub Sendi2c_byte
'      Teller = 0
'      Startdelay = 0
'      If Freqhop = 1 Then Gosub Startfreqhop
'      Setradio = 1
'    End If
'  End If

'  If I2c_byte = 16 Then
'    If Step_freq = 125 Then I2c_byte = 1
'    If Step_freq = 250 Then I2c_byte = 2
'    If Step_freq = 1000 Then I2c_byte = 3
'    If Step_freq = 10000 Then I2c_byte = 4
'    Gosub Sendi2c_byte
'    If I2c_commandmode = 1 Then
'      Waitms 7
'      Gosub Readi2c_byte
'      If I2c_byte > 4 Then I2c_byte = 4
'      If I2c_byte < 1 Then I2c_byte = 1
'      If I2c_byte = 1 Then Step_freq = 125
'      If I2c_byte = 2 Then Step_freq = 250
'      If I2c_byte = 3 Then Step_freq = 1000
'      If I2c_byte = 4 Then Step_freq = 10000
'      Gosub Sendi2c_byte
'    End If
'  End If

'  If I2c_byte = 17 Then
'    I2c_byte = Squ_level
'    Gosub Sendi2c_byte
'    If I2c_commandmode = 1 Then
'      Waitms 7
'      Gosub Readi2c_byte
'      Squ_level = I2c_byte
'      If Squ_level > 8 Then Squ_level = 8
'      I2c_byte = Squ_level
'      Gosub Sendi2c_byte
'      Teller = 0
'      Startdelay = 0
'      Setradio = 1
'    End If
'  End If

'  If I2c_byte = 18 Then
'    I2c_byte = Vol_level
'    Gosub Sendi2c_byte
'    If I2c_commandmode = 1 Then
'      Waitms 7
'      Gosub Readi2c_byte
'      Vol_level = I2c_byte
'      If Vol_level > 8 Then Vol_level = 8
'      I2c_byte = Vol_level
'      Gosub Sendi2c_byte
'      Teller = 0
'      Startdelay = 0
'      Setradio = 1
'    End If
'  End If

'  If I2c_byte = 19 Then
'    I2c_byte = Mod_level
'    Gosub Sendi2c_byte
'    If I2c_commandmode = 1 Then
'      Waitms 7
'      Gosub Readi2c_byte
'      Mod_level = I2c_byte
'      If Mod_level > 8 Then Mod_level = 8
'      I2c_byte = Mod_level
'      Gosub Sendi2c_byte
'      Teller = 0
'      Startdelay = 0
'      Setradio = 1
'    End If
'  End If

'  If I2c_byte = 20 Then
'    I2c_byte = Contrast - 128
'    Gosub Sendi2c_byte
'    If I2c_commandmode = 1 Then
'      Waitms 7
'      Gosub Readi2c_byte
'      Contrast = I2c_byte + 128
'      Gosub Sendi2c_byte
'      Glcdcmd 33 : Glcdcmd Contrast
'    End If
'  End If

'  If I2c_byte = 21 Then
'    I2c_byte = Tx_ctcss - 1
'    Gosub Sendi2c_byte
'    If I2c_commandmode = 1 Then
'      Waitms 7
'      Gosub Readi2c_byte
'      Tx_ctcss = I2c_byte + 1
'      Gosub Sendi2c_byte
'      Teller = 0
'      Startdelay = 0
'      Setradio = 1
'    End If
'  End If

'  If I2c_byte = 22 Then
'    I2c_byte = Rx_ctcss - 1
'    Gosub Sendi2c_byte
'    If I2c_commandmode = 1 Then
'      Waitms 7
'      Gosub Readi2c_byte
'      Rx_ctcss = I2c_byte + 1
'      Gosub Sendi2c_byte
'      Teller = 0
'      Startdelay = 0
'      Setradio = 1
'    End If
'  End If

'  If I2c_byte = 23 Then
'    I2c_byte = Usectcss
'    Gosub Sendi2c_byte
'    If I2c_commandmode = 1 Then
'      Waitms 7
'      Gosub Readi2c_byte
'      Usectcss = I2c_byte
'      If Usectcss > 3 Then Usectcss = 3
'      I2c_byte = Usectcss
'      Gosub Sendi2c_byte
'      Teller = 0
'      Startdelay = 0
'      Setradio = 1
'    End If
'  End If

'  If I2c_byte = 24 Then
'    I2c_byte = Buttonmode
'    Gosub Sendi2c_byte
'    If I2c_commandmode = 1 Then
'      Waitms 7
'      Gosub Readi2c_byte
'      Buttonmode = I2c_byte
'      If Buttonmode > 6 Then Buttonmode = 6
'      I2c_byte = Buttonmode
'      Gosub Sendi2c_byte
'    End If
'  End If

'  If I2c_byte = 25 Then
'    Freqvar = Frx - 1340000
'    Gosub Sendfreq
'    If I2c_commandmode = 1 Then
'      Waitms 20
'      Gosub Readi2c_byte
'      If I2c_byte < 0 Then I2c_byte = 255 - I2c_byte
'      Gosub Sendi2c_byte
'      Freqvar = I2c_byte * 256
'      Waitms 7
'      Gosub Readi2c_byte
'      If I2c_byte < 0 Then I2c_byte = 255 - I2c_byte
'      Gosub Sendi2c_byte
'      Freqvar = Freqvar + I2c_byte
'      Freqvar = Freqvar * 125
'      Frx = Freqvar + 1340000
'      Teller = 0
'      Scanchannel = 0
'      Startdelay = 0
'      Setradio = 1
'    End If
'  End If

'  If I2c_byte = 26 Then
'    Freqvar = Scanmin - 1340000
'    Gosub Sendfreq
'    If I2c_commandmode = 1 Then
'      Waitms 20
'      Gosub Readi2c_byte
'      If I2c_byte < 0 Then I2c_byte = 255 - I2c_byte
'      Gosub Sendi2c_byte
'      Freqvar = I2c_byte * 256
'      Waitms 7
'      Gosub Readi2c_byte
'      If I2c_byte < 0 Then I2c_byte = 255 - I2c_byte
'      Gosub Sendi2c_byte
'      Freqvar = Freqvar + I2c_byte
'      Freqvar = Freqvar * 125
'      Scanmin = Freqvar + 1340000
'    End If
'  End If
'  If I2c_byte = 27 Then
'    Freqvar = Scanmax - 1340000
'    Gosub Sendfreq
'    If I2c_commandmode = 1 Then
'      Waitms 20
'      Gosub Readi2c_byte
'      If I2c_byte < 0 Then I2c_byte = 255 - I2c_byte
'      Gosub Sendi2c_byte
'      Freqvar = I2c_byte * 256
'      Waitms 7
'      Gosub Readi2c_byte
'      If I2c_byte < 0 Then I2c_byte = 255 - I2c_byte
'      Gosub Sendi2c_byte
'      Freqvar = Freqvar + I2c_byte
'      Freqvar = Freqvar * 125
'      Scanmax = Freqvar + 1340000
'    End If
'  End If
'  If I2c_byte = 28 Then
'    Freqvar = Monifreq - 1340000
'    Gosub Sendfreq
'    If I2c_commandmode = 1 Then
'      Waitms 20
'      Gosub Readi2c_byte
'      If I2c_byte < 0 Then I2c_byte = 255 - I2c_byte
'      Gosub Sendi2c_byte
'      Freqvar = I2c_byte * 256
'      Waitms 7
'      Gosub Readi2c_byte
'      If I2c_byte < 0 Then I2c_byte = 255 - I2c_byte
'      Gosub Sendi2c_byte
'      Freqvar = Freqvar + I2c_byte
'      Freqvar = Freqvar * 125
'      Monifreq = Freqvar + 1340000
'    End If
'  End If
'  If I2c_byte = 29 Then
'    I2c_byte = Scanchannel
'    Gosub Sendi2c_byte
'    If I2c_commandmode = 1 Then
'      Waitms 7
'      Gosub Readi2c_byte
'      Scanchannel = I2c_byte
'      Gosub Sendi2c_byte
'      Teller = 0
'      Startdelay = 0
'      Setradio = 1
'    End If
'  End If
'  If I2c_byte = 30 Then
'    I2c_byte = 0
'    If Rx_led = 1 Then I2c_byte = I2c_byte + 1
'    If Tx_led = 1 Then I2c_byte = I2c_byte + 2
'    Gosub Sendi2c_byte
'    If I2c_commandmode = 1 Then
'      Waitms 7
'      Gosub Readi2c_byte
'      Gosub Sendi2c_byte
'    End If
'  End If
'  If I2c_byte = 31 Then
'    I2c_byte = 0
'    If I2c_commandmode = 0 Then
'      I2c_byte = 1
'      Gosub Getdefaults
'    End If
'    Gosub Sendi2c_byte
'    If I2c_commandmode = 1 Then
'      Waitms 7
'      Gosub Readi2c_byte
'      Gosub Saveeprom
'      Gosub Sendi2c_byte
'    End If
'  End If
'  If I2c_byte = 32 Then
'    I2c_byte = 1
'    Gosub Sendi2c_byte
'    If I2c_commandmode = 1 Then
'      Waitms 7
'      Gosub Readi2c_byte
'      Monifreq = Frx
'      Monifreqchannel = Scanchannel
'      I2c_byte = 1
'      Gosub Sendi2c_byte
'    End If
'  End If
'  If I2c_byte = 34 Then
'    I2c_byte = Dotransmit
'    Gosub Sendi2c_byte
'    If I2c_commandmode = 1 Then
'      Waitms 7
'      Gosub Readi2c_byte
'      If I2c_byte = 1 Then
'         I2ctransmit = 1
'      Else
'         I2ctransmit = 0
'      End If
'      Gosub Sendi2c_byte
'    End If
'  End If
Return

Sendfreq:
  Freqvar = Freqvar / 125

  Freqbyte_l = Freqvar
  Shift Freqvar , Right , 8
  Freqbyte_h = Freqvar

  Waitms 7
  I2c_byte = Freqbyte_h
  Gosub Sendi2c_byte

  Waitms 7
  I2c_byte = Freqbyte_l
  Gosub Sendi2c_byte
Return

Getdefaults:
  Frx = 1457500                                             'Default RX frequency
  Scanchannel = 29                                          'Default repeaterchannel
  Scanmin = 1445000                                         'Lowest scan frequency
  Scanminchannel = 0                                        'Lowest scan repeaterchannel
  Scanmax = 1460000                                         'Highest scan frequency
  Scanmaxchannel = 0                                        'Highest scan repeaterchannel
  Tx_ctcss = 1                                              'Ctcss frequency(0000 - 0038 = 1 - 39); 1 = "no CTCSS"
  Rx_ctcss = 1                                              'Ctcss frequency(0000 - 0038 = 1 - 39); 1 = "no CTCSS"
  Squ_level = 1                                             'Squelsh level
  Mod_level = 8                                             'Modulation level
  Vol_level = 8                                             'Volume
  Step_freq = 125                                           'Stepsize
  Doscan = 3                                                'Scanmode (0=no, 1 is on, 2= auto, 3=monitor)
  Doreverse = 0                                             'Reverse mode
  Monifreq = 1455750                                        'Monitor frequency
  Monifreqchannel = 1                                       'Monitor repeaterchannel
  If Frx < 4000000 Then
    Lastvhf = Frx                                           'Last VHF channel before swapping to UHF
    Lastvhfscanchannel = Scanchannel                        'Last VHF repeaterchannel before swapping to UHF
    Lastuhf = 4305000                                       'Last UHF channel before swapping to UHF
    Lastuhfscanchannel = 41                                 'Last UHF repeaterchannel before swapping to UHF
  Else
    Lastvhf = 1455000                                       'Last VHF channel before swapping to UHF
    Lastvhfscanchannel = 0                                  'Last VHF repeaterchannel before swapping to UHF
    Lastuhf = Frx                                           'Last UHF channel before swapping to UHF
    Lastuhfscanchannel = Scanchannel                        'Last UHF repeaterchannel before swapping to UHF
  End If
  Buttonmode = 3                                            'Long press function
  Contrast = 190
  I2c_channel = 22                                          '22
  If I2c_channel > 0 Then I2c_enabled = 1 Else I2c_enabled = 0
  I2c_active = I2c_enabled
  Usectcss = 3                                              '0=none,1=RX,2=TX,3=RTX
Return

Saveeprom:
  Frx_e = Frx
  Scanchannel_e = Scanchannel
  Scanmin_e = Scanmin
  Scanminchannel_e = Scanminchannel
  Scanmax_e = Scanmax
  Scanmaxchannel_e = Scanmaxchannel
  Tx_ctcss_e = Tx_ctcss
  Rx_ctcss_e = Rx_ctcss
  Squ_level_e = Squ_level
  Mod_level_e = Mod_level
  Vol_level_e = Vol_level
  Step_freq_e = Step_freq
  Doscan_e = Doscan
  Doreverse_e = Doreverse
  Monifreq_e = Monifreq
  Monifreqchannel_e = Monifreqchannel
  Lastvhf_e = Lastvhf
  Lastvhfscanchannel_e = Lastvhfscanchannel
  Lastuhf_e = Lastuhf
  Lastuhfscanchannel_e = Lastuhfscanchannel
  Buttonmode_e = Buttonmode
  Contrast_e = Contrast
  I2c_channel_e = I2c_channel
  Usectcss_e = Usectcss
Return

Loadeprom:
  Frx = Frx_e
  Scanchannel = Scanchannel_e
  Scanmin = Scanmin_e
  Scanminchannel = Scanminchannel_e
  Scanmax = Scanmax_e
  Scanmaxchannel = Scanmaxchannel_e
  Tx_ctcss = Tx_ctcss_e
  Rx_ctcss = Rx_ctcss_e
  Squ_level = Squ_level_e
  Mod_level = Mod_level_e
  Vol_level = Vol_level_e
  Step_freq = Step_freq_e
  Doscan = Doscan_e
  Doreverse = Doreverse_e
  Monifreq = Monifreq_e
  Monifreqchannel = Monifreqchannel_e
  Lastvhf = Lastvhf_e
  Lastvhfscanchannel = Lastvhfscanchannel_e
  Lastuhf = Lastuhf_e
  Lastuhfscanchannel = Lastuhfscanchannel_e
  Buttonmode = Buttonmode_e
  Contrast = Contrast_e
  I2c_channel = I2c_channel_e
  If I2c_channel > 0 Then I2c_enabled = 1 Else I2c_enabled = 0
  I2c_active = I2c_enabled
  Usectcss = Usectcss_e
Return


Plaatje:
'$BGF will put the bitmap into the program at this location
'$bgf "mcs.bgf"

$include "font5x5.font"
$include "font6x8.font"
'$include "font8x8.font"
$include "font10x16tt.font"
'$include "font11x13.font"

Repeaterchannels:
Data 0 , 1 , 3 , 3 , 3 , 3 , 4 , 5 , 5 , 5 , 6 , 6 , 7 , 7 , 8 , 9 , 9 , 10 , 10 , 11 , 11 , 11 , 12 , 13 , 13 , 13 , 14 , 15 , 15 , 15 , 16 , 17

Repeaters:
Data "DUMMY " , "PI3UTR" , "PI3ALK" , "PI3ZVL" , "PI3TWE" , "PI3SRT" , "PI3APM" , "PI3BOZ" , "PI3ZOD" , "PI3AMF" , "PI3FRL" , _
                                "PI3ZUT" , "PI3MEP" , "PI3BRD" , "PI3VLI" , "PI3RTD" , "PI3NOV" , "PI3VHP" , "PI3YMD" , "PI3HVN" , "PI3EHV" , _
                                "PI3DTC" , "PI3ZAZ" , "PI3GOE" , "PI3APD" , "PI3ZLB" , "PI3FLD" , "PI3NYM" , "PI3GRN" , "PI3RAZ" , "PI3ALM" , "PI3ASD"

Cities:
Data "Dummy      " , "IJsselstein" , "Alkmaar    " , "Overslag   " , "Eibergen   " , "Venlo      " , "Appingendam" , "Bergen/Zoom" , _
                                  "Emmen      " , "Amersfoort " , "Leeuwarden " , "Zutphen    " , "Meppel     " , "Breda      " , "Vlissingen " , _
                                  "Rotterdam  " , "'t Harde   " , "Vroomshoop " , "IJmuiden   " , "Heerenveen " , "Eindhoven  " , "Doetinchem " , _
                                  "Wormerveer " , "Goes       " , "Apeldoorn  " , "Geleen     " , "Lelystad   " , "Nijmegen   " , "Groningen  " , _
                                  "Zoetermeer " , "Almere     " , "Amsterdam  "

Ctcsstxnrs:
Data 0 , 5 , 9 , 1 , 5 , 3 , 7 , 9 , 1 , 1 , 7 , 1 , 7 , 3 , 9 , 9 , 5 , 5 , 9 , 7 , 3 , 5 , 9 , 9 , 5 , 3 , 5 , 5 , 7 , 9 , 1 , 9

Ctcssrxnrs:
Data 0 , 1 , 9 , 1 , 5 , 3 , 7 , 9 , 1 , 1 , 7 , 1 , 7 , 3 , 9 , 9 , 5 , 5 , 9 , 7 , 3 , 5 , 9 , 9 , 5 , 3 , 5 , 5 , 7 , 9 , 1 , 9

Uhfrepeaterchannels:
Data 0 , 1 , 1 , 2 , 4 , 4 , 5 , 5 , 5 , 6 , 6 , 6 , 7 , 8 , 10 , 12 , 12 , 12 , 14 , 16 , 16 , 17 , 17 , 18 , 18 , 19 , 19 , 19 , 20 , _
20 , 21 , 21 , 22 , 23 , 23 , 24 , 25 , 25 , 26 , 26 , 26 , 28 , 28 , 29 , 29 , 29 , 30 , 30 , 98

Uhfrepeaters:
Data "DUMMY  " , "PI2YSS" , "PI2GOR" , "PI2HVN" , "PI2ASN" , "PI2ANH" , "PI2JUT" , "PI2NEN" , "PI2ALW" , "PI2ZST" , "PI2MEP" , "PI2VLI" , "PI2DFT" , "PI2EHV" , "PI2NOS" , _
                                  "PI2APD" , "PI2RAZ" , "PI2WSN" , "PI2KMP" , "PI2ASD" , "PI2NLB" , "PI2BOZ" , "PI2TWR" , "PI2TWE" , "PI2SWK" , "PI2LDN" , "PI2SHB" , "PI2GRO" , "PI2HLM" , _
                                   "PI2AMF" , "PI2NOV" , "PI2RDH" , "PI2NON" , "PI2FLD" , "PI2KAR" , "PI2RTD" , "PI2NMG" , "PA1FP " , "PI2BRD" , "PI2ZAZ" , "PI2DZL" , "PI2HGL" , "PI2VNR" , "PI2DEV" , _
                                   "PI2RMD" , "PI2DEC" , "PI2ALK" , "PI2OSS" , "KAPPA"

Uhfcities:
Data "DUMMY      " , "Zutphen    " , "Gorinchem  " , "Heerenveen " , "Assen      " , "Arnhem     " , "Laag-Soeren" , "Nuenen     " , "Wijnaldum  " , "Zeist      " , _
                                 "Meppel     " , "Vlissingen " , "Delft      " , "Eindhoven  " , "Hilversum  " , "Apeldoorn  " , "Zoetermeer " , "Stadskanaal" , "Kampen     " , _
                                 "Amsterdam  " , "Gennep     " , "Bergen op Z" , "Barneveld  " , "Eibergen   " , "Gouda      " , "Leiden     " , "Den Bosch  " , "Groningen  " , _
                                  "Haarlem    " , "Amersfoort " , "t Harde    " , "Den Helder " , "Noord-Oost " , "Almere     " , "Bladel     " , "Rotterdam  " , "Nijmegen   " , "Hoorn      " , _
                                 "Breda      " , "Wormerveer " , "Delfzijl   " , "Den Haag   " , "Venray     " , "Deventer   " , "Melick     " , "Dordrecht  " , "Alkmaar    " , _
                                   "Oss        " , "JUTBERG"

Uhfctcsstxnrs:
Data 0 , 5 , 9 , 7 , 1 , 1 , 1 , 3 , 1 , 1 , 1 , 9 , 9 , 3 , 1 , 1 , 9 , 7 , 5 , 9 , 5 , 9 , 1 , 1 , 9 , 9 , 3 , 7 , 9 , 1 , 5 , 1 , 1 , 1 , 3 , 9 , 5 , 1 , 3 , 9 , 7 , 9 , 3 , 1 , 1 , 9 , 9 , 3 , 1

Uhfctcssrxnrs:
Data 0 , 5 , 9 , 7 , 1 , 1 , 1 , 3 , 1 , 1 , 1 , 9 , 9 , 3 , 1 , 1 , 9 , 7 , 5 , 9 , 5 , 9 , 1 , 1 , 9 , 9 , 3 , 7 , 9 , 1 , 5 , 1 , 1 , 1 , 3 , 9 , 5 , 1 , 3 , 9 , 7 , 9 , 3 , 1 , 1 , 9 , 9 , 3 , 1

Ctcsscodes:
Data "DUMY" , "0000" , "0001" , "0002" , "0003" , "0004" , "0005" , "0006" , "0007" , "0008" , "0009" , "0010" , "0011" , "0012" , _
                                      "0013" , "0014" , "0015" , "0016" , "0017" , "0018" , "0019" , "0020" , "0021" , "0022" , "0023" , "0024" , "0025" , "0026" , _
                                      "0027" , "0028" , "0029" , "0030" , "0031" , "0032" , "0033" , "0034" , "0035" , _
                                       "0036" , "0037" , "0038"

Ctcssfreqs:
Data "Dummy" , "None " , "67.0 " , "71.9 " , "74.4 " , "77.0 " , "79.7 " , "82.5 " , "85.4 " , "88.5 " , "91.5 " , "94.8 " , "97.4 " , "100.0" , _
                                      "103.5" , "107.2" , "110.9" , "114.8" , "118.8" , "123.0" , "127.3" , "131.8" , "136.5" , "141.3" , "146.2" , "151.4" , "156.7" , _
                                      "162.2" , "167.9" , "173.8" , "179.9" , "186.2" , "192.8" , "203.5" , "210.7" , "218.1" , "225.7" , "233.6" , "241.8" , "250.3"