-- Seed: 13275762795010174616,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (hxnygodml : in integer_vector(1 to 2); ttmjwerzl : buffer std_logic; ea : inout integer; kmyjgp : out boolean);
end e;

architecture oqgucy of e is
  
begin
  -- Multi-driven assignments
  ttmjwerzl <= '-';
end oqgucy;

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (d : inout std_logic_vector(0 to 2));
end s;

architecture wvzp of s is
  
begin
  -- Multi-driven assignments
  d <= "U-L";
  d <= "WL0";
end wvzp;

library ieee;
use ieee.std_logic_1164.all;

entity kgvlk is
  port (nkm : linkage std_logic_vector(3 to 1); amcfqrfoz : out time_vector(1 downto 2); dgqi : inout time);
end kgvlk;

architecture muwgdup of kgvlk is
  
begin
  -- Single-driven assignments
  dgqi <= 3_2_2 ns;
  amcfqrfoz <= (others => 0 ns);
end muwgdup;

entity jyrgpyjsg is
  port (kdnzkzqobh : in integer; wvatd : inout boolean_vector(3 to 3); qwad : linkage integer_vector(4 to 1));
end jyrgpyjsg;

library ieee;
use ieee.std_logic_1164.all;

architecture ihgtp of jyrgpyjsg is
  signal udk : boolean;
  signal yhsxlsejd : integer;
  signal xuwy : integer_vector(1 to 2);
  signal gddaoa : boolean;
  signal w : integer;
  signal uoopao : std_logic;
  signal rhamwxqwh : integer_vector(1 to 2);
begin
  ponc : entity work.e
    port map (hxnygodml => rhamwxqwh, ttmjwerzl => uoopao, ea => w, kmyjgp => gddaoa);
  dwjirj : entity work.e
    port map (hxnygodml => xuwy, ttmjwerzl => uoopao, ea => yhsxlsejd, kmyjgp => udk);
  
  -- Single-driven assignments
  wvatd <= (others => FALSE);
  rhamwxqwh <= (8#1242#, 4);
  
  -- Multi-driven assignments
  uoopao <= 'X';
  uoopao <= '-';
end ihgtp;



-- Seed after: 77205324878469972,17924494779688682807
