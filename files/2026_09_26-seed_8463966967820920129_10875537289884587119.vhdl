-- Seed: 8463966967820920129,10875537289884587119

entity ltcose is
  port (cbhnsltl : in severity_level; jyhwhtrz : buffer integer_vector(0 downto 3));
end ltcose;

architecture rw of ltcose is
  
begin
  
end rw;

library ieee;
use ieee.std_logic_1164.all;

entity ioihwb is
  port (bunszb : buffer std_logic; m : linkage integer; c : buffer severity_level);
end ioihwb;

architecture rkbkuyzapt of ioihwb is
  signal cgmjt : integer_vector(0 downto 3);
  signal egzdbd : integer_vector(0 downto 3);
  signal ukbqoup : severity_level;
begin
  h : entity work.ltcose
    port map (cbhnsltl => ukbqoup, jyhwhtrz => egzdbd);
  rgqmpgp : entity work.ltcose
    port map (cbhnsltl => c, jyhwhtrz => cgmjt);
  
  -- Single-driven assignments
  c <= ukbqoup;
  ukbqoup <= FAILURE;
  
  -- Multi-driven assignments
  bunszb <= bunszb;
  bunszb <= bunszb;
  bunszb <= '1';
end rkbkuyzapt;

entity rqpozme is
  port (zkwreisc : linkage real_vector(1 downto 2));
end rqpozme;

library ieee;
use ieee.std_logic_1164.all;

architecture mgxdclhff of rqpozme is
  signal gyehmvrle : integer_vector(0 downto 3);
  signal dhitkw : integer_vector(0 downto 3);
  signal src : severity_level;
  signal nvmzpbkh : integer;
  signal xqddmtlk : std_logic;
  signal otib : integer_vector(0 downto 3);
  signal trbcx : severity_level;
begin
  uiqvtg : entity work.ltcose
    port map (cbhnsltl => trbcx, jyhwhtrz => otib);
  ealzfhpmm : entity work.ioihwb
    port map (bunszb => xqddmtlk, m => nvmzpbkh, c => trbcx);
  tny : entity work.ltcose
    port map (cbhnsltl => src, jyhwhtrz => dhitkw);
  nftrpdldfk : entity work.ltcose
    port map (cbhnsltl => trbcx, jyhwhtrz => gyehmvrle);
  
  -- Single-driven assignments
  src <= trbcx;
  
  -- Multi-driven assignments
  xqddmtlk <= '0';
end mgxdclhff;

library ieee;
use ieee.std_logic_1164.all;

entity fzlbq is
  port (z : linkage real; dcmdqxk : out time; dlcq : buffer real; divzepel : inout std_logic_vector(3 to 1));
end fzlbq;

library ieee;
use ieee.std_logic_1164.all;

architecture pxaeqagrv of fzlbq is
  signal phwdw : severity_level;
  signal wttzqspkrq : integer;
  signal tnzadujrta : std_logic;
  signal zlspkkgnyq : integer_vector(0 downto 3);
  signal blyhkxci : severity_level;
  signal utgdrqx : integer_vector(0 downto 3);
  signal dc : severity_level;
  signal mceduntl : integer_vector(0 downto 3);
  signal srdxwk : severity_level;
begin
  vjxdyf : entity work.ltcose
    port map (cbhnsltl => srdxwk, jyhwhtrz => mceduntl);
  rej : entity work.ltcose
    port map (cbhnsltl => dc, jyhwhtrz => utgdrqx);
  oqqbkza : entity work.ltcose
    port map (cbhnsltl => blyhkxci, jyhwhtrz => zlspkkgnyq);
  haaqkb : entity work.ioihwb
    port map (bunszb => tnzadujrta, m => wttzqspkrq, c => phwdw);
  
  -- Single-driven assignments
  dc <= blyhkxci;
  dcmdqxk <= 1 sec;
  blyhkxci <= dc;
  
  -- Multi-driven assignments
  tnzadujrta <= '1';
  tnzadujrta <= 'U';
end pxaeqagrv;



-- Seed after: 10813467990129027088,10875537289884587119
