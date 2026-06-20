-- Seed: 6969705586445901767,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity kaidcon is
  port (goa : inout real; vvwxxrwx : out time; dcxpqhcetu : inout std_logic_vector(3 downto 0));
end kaidcon;

architecture auhg of kaidcon is
  
begin
  -- Single-driven assignments
  vvwxxrwx <= 0.2 fs;
  goa <= 16#6.56514#;
end auhg;

library ieee;
use ieee.std_logic_1164.all;

entity uzdqpmk is
  port (maqp : inout bit_vector(4 to 2); un : linkage std_logic_vector(4 downto 0); ozvfjfzaca : inout std_logic);
end uzdqpmk;

architecture uqnwn of uzdqpmk is
  
begin
  -- Single-driven assignments
  maqp <= (others => '0');
  
  -- Multi-driven assignments
  ozvfjfzaca <= 'Z';
end uqnwn;

library ieee;
use ieee.std_logic_1164.all;

entity oi is
  port (hj : in std_logic_vector(0 downto 2); qbfsrw : in character; uubsd : buffer real);
end oi;

library ieee;
use ieee.std_logic_1164.all;

architecture s of oi is
  signal dvepxof : std_logic;
  signal tesupr : std_logic_vector(4 downto 0);
  signal zfsflkil : bit_vector(4 to 2);
begin
  itvqfra : entity work.uzdqpmk
    port map (maqp => zfsflkil, un => tesupr, ozvfjfzaca => dvepxof);
  
  -- Single-driven assignments
  uubsd <= 2#1100.1001#;
  
  -- Multi-driven assignments
  tesupr <= "WXWZ-";
  dvepxof <= 'W';
  tesupr <= ('1', 'X', 'W', '1', '0');
end s;

entity jiufpywylo is
  port (dyz : buffer real; icqkx : linkage real; raarnvkvgn : in real);
end jiufpywylo;

library ieee;
use ieee.std_logic_1164.all;

architecture ttdq of jiufpywylo is
  signal m : std_logic_vector(3 downto 0);
  signal brmqvec : time;
  signal wt : time;
  signal kftazz : real;
  signal wkqptx : std_logic;
  signal jepkby : std_logic_vector(4 downto 0);
  signal hvdrg : bit_vector(4 to 2);
  signal ouvjfq : std_logic_vector(3 downto 0);
  signal woonbcrr : time;
  signal zdrqrvawgw : real;
begin
  ioreu : entity work.kaidcon
    port map (goa => zdrqrvawgw, vvwxxrwx => woonbcrr, dcxpqhcetu => ouvjfq);
  dmuru : entity work.uzdqpmk
    port map (maqp => hvdrg, un => jepkby, ozvfjfzaca => wkqptx);
  btuy : entity work.kaidcon
    port map (goa => kftazz, vvwxxrwx => wt, dcxpqhcetu => ouvjfq);
  tqoa : entity work.kaidcon
    port map (goa => dyz, vvwxxrwx => brmqvec, dcxpqhcetu => m);
  
  -- Multi-driven assignments
  ouvjfq <= ('L', '0', 'H', '0');
  ouvjfq <= ('H', 'L', 'U', 'H');
end ttdq;



-- Seed after: 17076526228563658709,17924494779688682807
