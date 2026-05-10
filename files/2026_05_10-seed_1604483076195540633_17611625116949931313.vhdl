-- Seed: 1604483076195540633,17611625116949931313



entity xpxejwptp is
  port (th : linkage time; ehbdwxrkp : linkage time);
end xpxejwptp;



architecture dpg of xpxejwptp is
  
begin
  
end dpg;



entity zpiassm is
  port (z : in severity_level);
end zpiassm;



architecture wjxwzm of zpiassm is
  signal oxvbbxnvl : time;
  signal flq : time;
  signal kzxbpaos : time;
  signal taqayjgecz : time;
begin
  lxaqstp : entity work.xpxejwptp
    port map (th => taqayjgecz, ehbdwxrkp => kzxbpaos);
  muvpxj : entity work.xpxejwptp
    port map (th => flq, ehbdwxrkp => oxvbbxnvl);
end wjxwzm;



entity cvj is
  port (liyd : out integer);
end cvj;



architecture vkgjc of cvj is
  signal xfkumewp : time;
begin
  dkm : entity work.xpxejwptp
    port map (th => xfkumewp, ehbdwxrkp => xfkumewp);
  erumdtdi : entity work.xpxejwptp
    port map (th => xfkumewp, ehbdwxrkp => xfkumewp);
end vkgjc;

library ieee;
use ieee.std_logic_1164.all;

entity puzas is
  port (qyikgvuxd : linkage time; vzim : buffer std_logic);
end puzas;



architecture omlmigmzg of puzas is
  signal vqhcrvdrl : severity_level;
  signal v : integer;
begin
  mmldksb : entity work.cvj
    port map (liyd => v);
  tt : entity work.zpiassm
    port map (z => vqhcrvdrl);
end omlmigmzg;



-- Seed after: 16534621933325882370,17611625116949931313
