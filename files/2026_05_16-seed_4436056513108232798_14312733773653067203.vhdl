-- Seed: 4436056513108232798,14312733773653067203



entity dtnbyjfd is
  port (gywfzac : out time; njjcfypoq : buffer real);
end dtnbyjfd;



architecture ymybvjxxok of dtnbyjfd is
  
begin
  
end ymybvjxxok;

library ieee;
use ieee.std_logic_1164.all;

entity dqm is
  port (mh : inout std_logic; cjxnhbn : out real; lpxafsu : out time; bzaiw : out std_logic);
end dqm;



architecture ohmngvhe of dqm is
  signal pupc : real;
  signal zsa : time;
begin
  dwlwnw : entity work.dtnbyjfd
    port map (gywfzac => zsa, njjcfypoq => pupc);
  j : entity work.dtnbyjfd
    port map (gywfzac => lpxafsu, njjcfypoq => cjxnhbn);
end ohmngvhe;

library ieee;
use ieee.std_logic_1164.all;

entity bupurigjl is
  port (ysfljcrnvm : inout real; wlolcst : buffer time; gyiomqtlx : out std_logic; aaumqh : inout real);
end bupurigjl;



architecture ysuepcfrpj of bupurigjl is
  signal xotxf : real;
  signal mnoyn : time;
begin
  pktq : entity work.dqm
    port map (mh => gyiomqtlx, cjxnhbn => aaumqh, lpxafsu => mnoyn, bzaiw => gyiomqtlx);
  lvfpqmr : entity work.dtnbyjfd
    port map (gywfzac => wlolcst, njjcfypoq => xotxf);
end ysuepcfrpj;



-- Seed after: 6441514028035348878,14312733773653067203
