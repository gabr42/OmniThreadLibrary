# python -I dllunload.py <folder with UnloadHost.exe> [repeat]
import os, subprocess, sys
from concurrent.futures import ThreadPoolExecutor

DIR = os.path.abspath(sys.argv[1])
REPEAT = int(sys.argv[2]) if len(sys.argv) > 2 else 6
POOLS = {0: 'global running', 2: 'global idle', 1: 'private running', 3: 'private idle', 5: 'leaked running',
         7: 'leaked idle', 8: 'parallel', 10: 'parallel idle', 18: 'own threads', 67: 'idle timeout',
         128: 'own DllProc', 130: 'own DllProc idle', 133: 'own DllProc leaked', 256: 'init', 258: 'init idle',
         261: 'init leaked', 513: 'owned running', 515: 'owned idle'}
CASES = [(m, n, h) for m, n in POOLS.items() for h in ('free', 'exit', 'shutdown')
         if (m, h) != (8, 'exit')]  # ExitProcess kills threads that are allocating memory; RTL then hangs (without OTL, too)
CASES += [(33, 'stuck', 'stuck'), (32, 'stuck global', 'stuck'), (33, 'stuck', 'exit')]


def run(a):
    mode, how, rounds = a
    p = subprocess.Popen([os.path.join(DIR, 'UnloadHost.exe'), str(mode), how, str(rounds)], cwd=DIR,
                         stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                         creationflags=0x08000000)
    try:
        return 'rc=%x' % (p.wait(60) & 0xffffffff)
    except subprocess.TimeoutExpired:
        p.kill()
        return 'HANG'


ok = True
for mode, name, how in CASES:
    with ThreadPoolExecutor(6) as ex:
        res = list(ex.map(run, [(mode, how, 1 if how in ('exit', 'stuck') else 3)] * REPEAT))
    counts = {r: res.count(r) for r in sorted(set(res))}
    ok = ok and list(counts) == ['rc=0']
    print('%-19s %-9s %s' % (name, how, counts), flush=True)
print('OK' if ok else 'FAILED')
sys.exit(0 if ok else 1)
