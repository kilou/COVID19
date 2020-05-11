---
title: "Doc Windows & système"
author: "Bastien Trächsel"
date: "4/22/2020"
output:
  pdf_document: default
  html_document: default
---

### Bios setup (to allow virtual machine)

At system boot, press 'DEL' or 'F2'.

Go to your UEFI Bios, under the 'cpu advanced configuration' you will need to enable AMD-V or VT-x(intel).

Depending on your system memory, you might want to disable SMT/HT to increase the ratio memory/thread.


### Installing Hyper-V in windows 10

Go to 'add windows functionalities' 
by typing it while the start menu is open and install the full Hyper-V component.

Reboot.

Download an image (.iso) http://mirror.init7.net/ubuntu-releases/20.04/

Launch Hyper-V and 'new'->'virtual computer'.

Select generation 1 for the Bios.

Put the .iso as boot media.


### Hyper-V network parameters

You need your virtual machine to access your LAN directly in order to be forwarded traffic on selected ports.

You need to create a new switch and set the ip to external.

Then, you need to edit the parameters of your virtual machine in order to use the newly created switch.

That way, the virtual machine is going to receive an IP address from your router.

While in the parameters tab, you may also want to set the number of cores and memory available to your virtual machine.
