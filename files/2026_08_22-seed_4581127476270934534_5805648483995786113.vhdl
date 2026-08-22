-- Seed: 4581127476270934534,5805648483995786113

entity ahurp is
  port (tbhf : buffer boolean_vector(1 downto 0));
end ahurp;

architecture lhcagezmn of ahurp is
  
begin
  -- Single-driven assignments
  tbhf <= (TRUE, FALSE);
end lhcagezmn;

entity vkdxzay is
  port (br : linkage time);
end vkdxzay;

architecture ggbsl of vkdxzay is
  signal qxrbo : boolean_vector(1 downto 0);
  signal vtiox : boolean_vector(1 downto 0);
begin
  yrrpqdvwes : entity work.ahurp
    port map (tbhf => vtiox);
  wy : entity work.ahurp
    port map (tbhf => qxrbo);
end ggbsl;

library ieee;
use ieee.std_logic_1164.all;

entity wxr is
  port (rhjq : out std_logic_vector(1 downto 0); ubz : linkage time; obaavgpuso : buffer std_logic_vector(1 to 3); iqak : in std_logic);
end wxr;

architecture mdielebc of wxr is
  signal wgbrbrwnq : time;
  signal tz : time;
  signal snknb : boolean_vector(1 downto 0);
begin
  ngtz : entity work.ahurp
    port map (tbhf => snknb);
  swcijzmj : entity work.vkdxzay
    port map (br => tz);
  y : entity work.vkdxzay
    port map (br => wgbrbrwnq);
  b : entity work.vkdxzay
    port map (br => ubz);
  
  -- Multi-driven assignments
  obaavgpuso <= obaavgpuso;
end mdielebc;



-- Seed after: 1834908446472520535,5805648483995786113
