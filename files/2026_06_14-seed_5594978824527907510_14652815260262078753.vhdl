-- Seed: 5594978824527907510,14652815260262078753

entity kuyd is
  port (gb : buffer real; ci : out real_vector(1 to 2); esaeqz : linkage integer);
end kuyd;

architecture nyt of kuyd is
  
begin
  
end nyt;

library ieee;
use ieee.std_logic_1164.all;

entity oy is
  port (xcunjv : in std_logic; maqda : out time; usnb : buffer std_logic_vector(1 downto 0); zybrldszw : buffer std_logic_vector(1 downto 2));
end oy;

architecture gyxfq of oy is
  
begin
  -- Multi-driven assignments
  zybrldszw <= (others => '0');
end gyxfq;

library ieee;
use ieee.std_logic_1164.all;

entity errlpq is
  port (xlhluxm : out real; t : in real_vector(4 downto 0); obk : buffer std_logic_vector(0 downto 4); kcxa : out integer);
end errlpq;

architecture eofw of errlpq is
  signal mnay : real_vector(1 to 2);
  signal ywi : real;
  signal atpoqzpxl : integer;
  signal vvvomfmieu : real_vector(1 to 2);
  signal sdk : real;
begin
  utwfgimeog : entity work.kuyd
    port map (gb => sdk, ci => vvvomfmieu, esaeqz => atpoqzpxl);
  kylvo : entity work.kuyd
    port map (gb => ywi, ci => mnay, esaeqz => kcxa);
  
  -- Single-driven assignments
  xlhluxm <= 8#24705.11320#;
  
  -- Multi-driven assignments
  obk <= (others => '0');
  obk <= (others => '0');
  obk <= (others => '0');
end eofw;



-- Seed after: 14600330565944820426,14652815260262078753
