-- Seed: 17639261416526020894,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity iqgvchmdd is
  port (do : inout real; obfod : in std_logic; hfo : inout bit_vector(0 downto 2); ovvvk : in real);
end iqgvchmdd;

architecture vtwhuwcgk of iqgvchmdd is
  
begin
  -- Single-driven assignments
  do <= 16#C_F_C.4#;
  hfo <= (others => '0');
end vtwhuwcgk;

library ieee;
use ieee.std_logic_1164.all;

entity kszslp is
  port (sbglorte : in real; i : inout std_logic_vector(4 downto 2); xgvuudt : linkage std_logic_vector(1 downto 3));
end kszslp;

library ieee;
use ieee.std_logic_1164.all;

architecture kvwdo of kszslp is
  signal fynlwoqzy : real;
  signal siii : bit_vector(0 downto 2);
  signal xgwx : std_logic;
  signal oqcypajaau : bit_vector(0 downto 2);
  signal szfqdw : real;
  signal n : real;
  signal odc : bit_vector(0 downto 2);
  signal rhxbuy : std_logic;
  signal pgkeaf : real;
begin
  hsuyq : entity work.iqgvchmdd
    port map (do => pgkeaf, obfod => rhxbuy, hfo => odc, ovvvk => n);
  b : entity work.iqgvchmdd
    port map (do => szfqdw, obfod => rhxbuy, hfo => oqcypajaau, ovvvk => sbglorte);
  sskuz : entity work.iqgvchmdd
    port map (do => n, obfod => xgwx, hfo => siii, ovvvk => fynlwoqzy);
  
  -- Single-driven assignments
  fynlwoqzy <= 16#7_D.B_0_F_4_4#;
  
  -- Multi-driven assignments
  rhxbuy <= '1';
  rhxbuy <= 'H';
  i <= "XL1";
  i <= ('H', '0', 'W');
end kvwdo;

library ieee;
use ieee.std_logic_1164.all;

entity ea is
  port (kewsxe : buffer std_logic_vector(1 to 2); rcmyoi : in std_logic_vector(3 downto 0); bkjh : linkage real; jmcff : buffer boolean);
end ea;

library ieee;
use ieee.std_logic_1164.all;

architecture ungifdl of ea is
  signal qxz : std_logic_vector(1 downto 3);
  signal axsbhtf : std_logic_vector(4 downto 2);
  signal rdejsa : bit_vector(0 downto 2);
  signal lgxbkcb : std_logic;
  signal nyu : real;
begin
  lrflxd : entity work.iqgvchmdd
    port map (do => nyu, obfod => lgxbkcb, hfo => rdejsa, ovvvk => nyu);
  spselvqq : entity work.kszslp
    port map (sbglorte => nyu, i => axsbhtf, xgvuudt => qxz);
  
  -- Single-driven assignments
  jmcff <= FALSE;
  
  -- Multi-driven assignments
  qxz <= "";
  qxz <= (others => '0');
  kewsxe <= ('L', 'L');
end ungifdl;



-- Seed after: 4768413757554417349,6882842853887419669
