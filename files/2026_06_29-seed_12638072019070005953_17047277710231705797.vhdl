-- Seed: 12638072019070005953,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity cjkgt is
  port (dzla : out std_logic_vector(1 to 1));
end cjkgt;

architecture bbz of cjkgt is
  
begin
  -- Multi-driven assignments
  dzla <= "X";
  dzla <= (others => 'Z');
  dzla <= (others => '0');
  dzla <= "L";
end bbz;

entity qgdzx is
  port (hd : inout real);
end qgdzx;

library ieee;
use ieee.std_logic_1164.all;

architecture butlgdhy of qgdzx is
  signal ioq : std_logic_vector(1 to 1);
begin
  pktaexjgbi : entity work.cjkgt
    port map (dzla => ioq);
  
  -- Single-driven assignments
  hd <= 2#01000.101#;
  
  -- Multi-driven assignments
  ioq <= "U";
  ioq <= (others => '-');
end butlgdhy;

entity wgrihlpgi is
  port (b : out time);
end wgrihlpgi;

library ieee;
use ieee.std_logic_1164.all;

architecture zxhwqy of wgrihlpgi is
  signal o : std_logic_vector(1 to 1);
begin
  mjd : entity work.cjkgt
    port map (dzla => o);
  
  -- Single-driven assignments
  b <= 1 hr;
  
  -- Multi-driven assignments
  o <= "Z";
end zxhwqy;



-- Seed after: 13256760963399435415,17047277710231705797
