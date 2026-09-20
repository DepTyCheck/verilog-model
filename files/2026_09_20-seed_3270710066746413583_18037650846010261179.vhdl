-- Seed: 3270710066746413583,18037650846010261179

entity zckozcp is
  port (cpqm : buffer real_vector(4 downto 2); xcw : in real_vector(4 to 2); xudr : linkage integer);
end zckozcp;

architecture ywya of zckozcp is
  
begin
  -- Single-driven assignments
  cpqm <= (2#1.0#, 2#0001.0#, 16#7_7.EE#);
end ywya;

library ieee;
use ieee.std_logic_1164.all;

entity gagmisvcni is
  port (kbykn : linkage integer; tn : inout std_logic_vector(1 to 1); mwzghixj : linkage integer; muibyx : buffer integer);
end gagmisvcni;

architecture yvrasaabyu of gagmisvcni is
  signal cbbfvwguy : integer;
  signal izonw : real_vector(4 to 2);
  signal otgsyqqv : real_vector(4 downto 2);
  signal amrrgzb : integer;
  signal mjdnwgcuk : real_vector(4 to 2);
  signal inteebmja : real_vector(4 downto 2);
begin
  jsyxg : entity work.zckozcp
    port map (cpqm => inteebmja, xcw => mjdnwgcuk, xudr => amrrgzb);
  xjafhel : entity work.zckozcp
    port map (cpqm => otgsyqqv, xcw => izonw, xudr => cbbfvwguy);
  
  -- Single-driven assignments
  muibyx <= 2#0_1_0#;
  mjdnwgcuk <= mjdnwgcuk;
  
  -- Multi-driven assignments
  tn <= tn;
  tn <= tn;
end yvrasaabyu;



-- Seed after: 1294343731147531541,18037650846010261179
