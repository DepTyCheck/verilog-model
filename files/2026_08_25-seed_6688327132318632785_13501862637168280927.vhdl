-- Seed: 6688327132318632785,13501862637168280927

library ieee;
use ieee.std_logic_1164.all;

entity vjqcchsy is
  port (zqji : out bit; rmjwkjwf : out real; resovoljb : in std_logic);
end vjqcchsy;

architecture su of vjqcchsy is
  
begin
  -- Single-driven assignments
  zqji <= '1';
  rmjwkjwf <= 04442.1022;
end su;

entity ishmvrryz is
  port (cuig : buffer real);
end ishmvrryz;

library ieee;
use ieee.std_logic_1164.all;

architecture xlctyjylw of ishmvrryz is
  signal ucsztns : real;
  signal ln : bit;
  signal ny : std_logic;
  signal qqld : bit;
  signal poaoj : real;
  signal xi : bit;
  signal pzxcisjhz : std_logic;
  signal enspwwzbf : real;
  signal ujttkug : bit;
begin
  qalkbm : entity work.vjqcchsy
    port map (zqji => ujttkug, rmjwkjwf => enspwwzbf, resovoljb => pzxcisjhz);
  hjvomwa : entity work.vjqcchsy
    port map (zqji => xi, rmjwkjwf => poaoj, resovoljb => pzxcisjhz);
  lkfr : entity work.vjqcchsy
    port map (zqji => qqld, rmjwkjwf => cuig, resovoljb => ny);
  k : entity work.vjqcchsy
    port map (zqji => ln, rmjwkjwf => ucsztns, resovoljb => pzxcisjhz);
  
  -- Multi-driven assignments
  pzxcisjhz <= ny;
end xlctyjylw;

entity akwhttrzk is
  port (tqshr : buffer time; lyzlahco : inout integer; mzm : out severity_level; kgiyxklwmg : buffer character);
end akwhttrzk;

library ieee;
use ieee.std_logic_1164.all;

architecture hhpsnd of akwhttrzk is
  signal irbxxvkvdu : std_logic;
  signal dezsa : real;
  signal ys : bit;
begin
  yzb : entity work.vjqcchsy
    port map (zqji => ys, rmjwkjwf => dezsa, resovoljb => irbxxvkvdu);
  
  -- Single-driven assignments
  kgiyxklwmg <= 'g';
  mzm <= WARNING;
  lyzlahco <= 3;
  tqshr <= tqshr;
  
  -- Multi-driven assignments
  irbxxvkvdu <= 'X';
  irbxxvkvdu <= 'L';
end hhpsnd;



-- Seed after: 12340936828040411230,13501862637168280927
