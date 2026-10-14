-- Seed: 15063197958875644985,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity fcdkug is
  port (n : out real; tzjxoe : inout std_logic; gbwkyoqt : in std_logic);
end fcdkug;

architecture u of fcdkug is
  
begin
  
end u;

entity l is
  port (koqlpxnqt : inout severity_level);
end l;

library ieee;
use ieee.std_logic_1164.all;

architecture jhohk of l is
  signal cdflahdrec : std_logic;
  signal lace : real;
  signal sjygzjm : std_logic;
  signal d : std_logic;
  signal sm : real;
begin
  xgbjsudu : entity work.fcdkug
    port map (n => sm, tzjxoe => d, gbwkyoqt => sjygzjm);
  jdvmh : entity work.fcdkug
    port map (n => lace, tzjxoe => cdflahdrec, gbwkyoqt => d);
  
  -- Single-driven assignments
  koqlpxnqt <= koqlpxnqt;
end jhohk;

library ieee;
use ieee.std_logic_1164.all;

entity wjkfeekgwr is
  port (bhgjgsavj : out time; xwuvd : inout real; bqffdaqqb : inout std_logic; sovec : buffer bit);
end wjkfeekgwr;

architecture z of wjkfeekgwr is
  signal zn : severity_level;
begin
  ya : entity work.l
    port map (koqlpxnqt => zn);
  pvofkj : entity work.fcdkug
    port map (n => xwuvd, tzjxoe => bqffdaqqb, gbwkyoqt => bqffdaqqb);
  
  -- Single-driven assignments
  sovec <= '0';
  bhgjgsavj <= 2_1.2 fs;
end z;



-- Seed after: 13580943518896345891,2287986438313874379
