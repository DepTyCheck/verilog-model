-- Seed: 7849516235008189728,6562834012285671529

entity l is
  port (vvyopne : linkage integer_vector(2 downto 3));
end l;

architecture zgoafp of l is
  
begin
  
end zgoafp;

entity rk is
  port (uhfhmpq : linkage integer; hqdlqdl : out integer);
end rk;

architecture adccr of rk is
  signal xxqrawgsaz : integer_vector(2 downto 3);
  signal qiavkxni : integer_vector(2 downto 3);
begin
  pdcgjrojxk : entity work.l
    port map (vvyopne => qiavkxni);
  rzcrnvjadi : entity work.l
    port map (vvyopne => xxqrawgsaz);
  
  -- Single-driven assignments
  hqdlqdl <= hqdlqdl;
end adccr;

library ieee;
use ieee.std_logic_1164.all;

entity auauravctp is
  port (kuv : inout std_logic);
end auauravctp;

architecture kggeimzo of auauravctp is
  
begin
  -- Multi-driven assignments
  kuv <= 'Z';
end kggeimzo;

entity rnlqjvpvr is
  port (fdffbt : buffer integer);
end rnlqjvpvr;

library ieee;
use ieee.std_logic_1164.all;

architecture xwsxcb of rnlqjvpvr is
  signal ubnyhbfppc : integer_vector(2 downto 3);
  signal amuwgdpv : integer;
  signal enblucrfe : std_logic;
  signal maewkmpfrm : integer;
  signal mkedrajuwu : integer;
begin
  oo : entity work.rk
    port map (uhfhmpq => mkedrajuwu, hqdlqdl => maewkmpfrm);
  e : entity work.auauravctp
    port map (kuv => enblucrfe);
  nvwumiorlz : entity work.rk
    port map (uhfhmpq => fdffbt, hqdlqdl => amuwgdpv);
  drtpn : entity work.l
    port map (vvyopne => ubnyhbfppc);
  
  -- Multi-driven assignments
  enblucrfe <= 'Z';
  enblucrfe <= '0';
  enblucrfe <= enblucrfe;
  enblucrfe <= enblucrfe;
end xwsxcb;



-- Seed after: 17356645271738358244,6562834012285671529
