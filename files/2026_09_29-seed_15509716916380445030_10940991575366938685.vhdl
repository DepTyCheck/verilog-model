-- Seed: 15509716916380445030,10940991575366938685

library ieee;
use ieee.std_logic_1164.all;

entity gipak is
  port (mpswax : linkage integer; lslsq : in std_logic_vector(4 downto 4); f : in std_logic_vector(1 downto 4); fxnjprvgor : out std_logic);
end gipak;

architecture igmyizxm of gipak is
  
begin
  -- Multi-driven assignments
  fxnjprvgor <= fxnjprvgor;
  fxnjprvgor <= fxnjprvgor;
end igmyizxm;

entity b is
  port (suqslbmilg : out time; xgtb : in integer);
end b;

library ieee;
use ieee.std_logic_1164.all;

architecture crbrlmaut of b is
  signal pvewnafa : std_logic;
  signal ja : integer;
  signal imsrtcqa : std_logic;
  signal maetdu : std_logic_vector(1 downto 4);
  signal szdsxej : std_logic_vector(4 downto 4);
  signal h : integer;
begin
  ti : entity work.gipak
    port map (mpswax => h, lslsq => szdsxej, f => maetdu, fxnjprvgor => imsrtcqa);
  oejryk : entity work.gipak
    port map (mpswax => ja, lslsq => szdsxej, f => maetdu, fxnjprvgor => pvewnafa);
  
  -- Single-driven assignments
  suqslbmilg <= suqslbmilg;
  
  -- Multi-driven assignments
  szdsxej <= szdsxej;
  maetdu <= (others => '0');
  imsrtcqa <= imsrtcqa;
end crbrlmaut;

entity c is
  port (rgkrfjx : out integer_vector(2 to 1));
end c;

library ieee;
use ieee.std_logic_1164.all;

architecture fl of c is
  signal ukxbx : time;
  signal yboag : std_logic_vector(1 downto 4);
  signal cqymst : integer;
  signal fdrxujpscd : std_logic;
  signal axmguixod : std_logic_vector(1 downto 4);
  signal qsxlhbwy : std_logic_vector(4 downto 4);
  signal aym : integer;
begin
  qmsobk : entity work.gipak
    port map (mpswax => aym, lslsq => qsxlhbwy, f => axmguixod, fxnjprvgor => fdrxujpscd);
  tjleihvh : entity work.gipak
    port map (mpswax => cqymst, lslsq => qsxlhbwy, f => yboag, fxnjprvgor => fdrxujpscd);
  maoe : entity work.b
    port map (suqslbmilg => ukxbx, xgtb => cqymst);
  
  -- Multi-driven assignments
  fdrxujpscd <= fdrxujpscd;
  qsxlhbwy <= (others => 'H');
  qsxlhbwy <= qsxlhbwy;
  qsxlhbwy <= (others => 'L');
end fl;



-- Seed after: 8253051811791956389,10940991575366938685
