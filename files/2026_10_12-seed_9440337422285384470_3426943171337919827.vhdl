-- Seed: 9440337422285384470,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity kgzesnv is
  port (l : in std_logic; qkcuxeg : inout bit_vector(1 downto 2));
end kgzesnv;

architecture bybldvjngp of kgzesnv is
  
begin
  -- Single-driven assignments
  qkcuxeg <= (others => '0');
end bybldvjngp;

library ieee;
use ieee.std_logic_1164.all;

entity xl is
  port (xscsnvntp : out integer; afgliun : in bit; kjpdnrqyf : linkage std_logic);
end xl;

library ieee;
use ieee.std_logic_1164.all;

architecture fajlekib of xl is
  signal qgaqfwhvt : bit_vector(1 downto 2);
  signal fikrgwn : std_logic;
  signal ucrkd : bit_vector(1 downto 2);
  signal i : bit_vector(1 downto 2);
  signal tgvpq : std_logic;
  signal k : bit_vector(1 downto 2);
  signal xyph : std_logic;
begin
  kxpdlih : entity work.kgzesnv
    port map (l => xyph, qkcuxeg => k);
  bniihdct : entity work.kgzesnv
    port map (l => tgvpq, qkcuxeg => i);
  wfjdju : entity work.kgzesnv
    port map (l => xyph, qkcuxeg => ucrkd);
  wbneuok : entity work.kgzesnv
    port map (l => fikrgwn, qkcuxeg => qgaqfwhvt);
  
  -- Single-driven assignments
  xscsnvntp <= 8#3_5#;
  
  -- Multi-driven assignments
  tgvpq <= 'X';
end fajlekib;



-- Seed after: 1236393600044732947,3426943171337919827
