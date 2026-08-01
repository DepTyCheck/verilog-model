-- Seed: 10551403673092693090,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (ytanmtxqmx : out std_logic_vector(3 to 0); mqpp : inout time; iwezxlalpx : out std_logic; rwdasrrl : out std_logic_vector(2 to 2));
end m;

architecture jzxgyf of m is
  
begin
  -- Single-driven assignments
  mqpp <= 4 min;
  
  -- Multi-driven assignments
  rwdasrrl <= "0";
  rwdasrrl <= rwdasrrl;
end jzxgyf;

library ieee;
use ieee.std_logic_1164.all;

entity lav is
  port (hhbyvc : out std_logic_vector(4 to 0); gp : in std_logic_vector(4 downto 0));
end lav;

architecture hinxvt of lav is
  
begin
  -- Multi-driven assignments
  hhbyvc <= (others => '0');
  hhbyvc <= hhbyvc;
end hinxvt;

entity fc is
  port (n : buffer integer_vector(2 to 2); ekoxf : inout real; xo : in boolean; cfeokiisrf : inout integer);
end fc;

library ieee;
use ieee.std_logic_1164.all;

architecture eg of fc is
  signal vcl : std_logic_vector(2 to 2);
  signal wzyglt : std_logic;
  signal uxu : time;
  signal nj : std_logic_vector(4 downto 0);
  signal blgjbek : std_logic_vector(3 to 0);
begin
  pgejdfjc : entity work.lav
    port map (hhbyvc => blgjbek, gp => nj);
  y : entity work.m
    port map (ytanmtxqmx => blgjbek, mqpp => uxu, iwezxlalpx => wzyglt, rwdasrrl => vcl);
  
  -- Single-driven assignments
  n <= (others => 2);
end eg;

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (urgfqg : out std_logic; arifok : inout boolean; mzb : buffer std_logic);
end s;

library ieee;
use ieee.std_logic_1164.all;

architecture oemv of s is
  signal cqqaeat : std_logic_vector(2 to 2);
  signal rwrc : std_logic;
  signal ntsbljjei : time;
  signal pn : std_logic_vector(3 to 0);
begin
  tquibqo : entity work.m
    port map (ytanmtxqmx => pn, mqpp => ntsbljjei, iwezxlalpx => rwrc, rwdasrrl => cqqaeat);
  
  -- Single-driven assignments
  arifok <= TRUE;
  
  -- Multi-driven assignments
  mzb <= '-';
  cqqaeat <= cqqaeat;
end oemv;



-- Seed after: 12021515826165918276,4292249356257567981
