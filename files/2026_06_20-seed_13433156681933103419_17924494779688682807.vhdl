-- Seed: 13433156681933103419,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity zhqgp is
  port (zegckz : in std_logic; slwdyx : linkage real; biqne : buffer string(1 downto 2); fmfbjjlq : inout std_logic);
end zhqgp;

architecture y of zhqgp is
  
begin
  -- Single-driven assignments
  biqne <= (others => ' ');
  
  -- Multi-driven assignments
  fmfbjjlq <= 'W';
  fmfbjjlq <= 'X';
end y;

library ieee;
use ieee.std_logic_1164.all;

entity edrhlybjfs is
  port (oqcw : in std_logic_vector(0 to 4); intdrfw : inout bit_vector(3 to 1));
end edrhlybjfs;

library ieee;
use ieee.std_logic_1164.all;

architecture fyybxxq of edrhlybjfs is
  signal dp : std_logic;
  signal sdakmvuq : string(1 downto 2);
  signal pp : real;
  signal pgdwwtnpd : std_logic;
  signal pfwu : string(1 downto 2);
  signal iiqkfrasz : real;
  signal ruzx : std_logic;
begin
  rpxwpqv : entity work.zhqgp
    port map (zegckz => ruzx, slwdyx => iiqkfrasz, biqne => pfwu, fmfbjjlq => pgdwwtnpd);
  cakcmz : entity work.zhqgp
    port map (zegckz => pgdwwtnpd, slwdyx => pp, biqne => sdakmvuq, fmfbjjlq => dp);
  
  -- Single-driven assignments
  intdrfw <= (others => '0');
  
  -- Multi-driven assignments
  ruzx <= '0';
  ruzx <= '0';
  dp <= 'H';
  ruzx <= 'W';
end fyybxxq;

library ieee;
use ieee.std_logic_1164.all;

entity iqzrpi is
  port (lw : buffer time; bxcdnxpz : in std_logic; gskazjw : buffer real);
end iqzrpi;

library ieee;
use ieee.std_logic_1164.all;

architecture owzm of iqzrpi is
  signal gn : string(1 downto 2);
  signal pn : real;
  signal vx : std_logic;
  signal ertjlynzsc : bit_vector(3 to 1);
  signal zn : std_logic_vector(0 to 4);
begin
  wrxtljh : entity work.edrhlybjfs
    port map (oqcw => zn, intdrfw => ertjlynzsc);
  locgklcpq : entity work.zhqgp
    port map (zegckz => vx, slwdyx => pn, biqne => gn, fmfbjjlq => vx);
  
  -- Single-driven assignments
  gskazjw <= 3.2_3_0_2_0;
  
  -- Multi-driven assignments
  vx <= 'W';
  vx <= '0';
end owzm;

library ieee;
use ieee.std_logic_1164.all;

entity q is
  port (lemick : out bit_vector(4 downto 0); afiqyjundz : out time; xb : in std_logic_vector(3 downto 4));
end q;

library ieee;
use ieee.std_logic_1164.all;

architecture dtbhdpl of q is
  signal slsiqvc : real;
  signal yjmwemzpw : std_logic;
  signal kvavqy : time;
begin
  qqtmguakgn : entity work.iqzrpi
    port map (lw => kvavqy, bxcdnxpz => yjmwemzpw, gskazjw => slsiqvc);
  
  -- Single-driven assignments
  lemick <= ('1', '0', '1', '0', '1');
  afiqyjundz <= 16#F_0# ms;
  
  -- Multi-driven assignments
  yjmwemzpw <= '-';
  yjmwemzpw <= 'H';
  yjmwemzpw <= 'W';
end dtbhdpl;



-- Seed after: 14420948762383191683,17924494779688682807
