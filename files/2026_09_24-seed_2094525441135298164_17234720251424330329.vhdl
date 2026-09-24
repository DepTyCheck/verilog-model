-- Seed: 2094525441135298164,17234720251424330329

library ieee;
use ieee.std_logic_1164.all;

entity rebz is
  port (ecuoyvr : buffer time_vector(3 downto 2); fdgikbwkd : in std_logic_vector(0 downto 3); mgxaa : out bit_vector(2 to 0));
end rebz;

architecture gwldwszfwp of rebz is
  
begin
  -- Single-driven assignments
  mgxaa <= (others => '0');
  ecuoyvr <= ecuoyvr;
end gwldwszfwp;

library ieee;
use ieee.std_logic_1164.all;

entity ndakh is
  port (lusvabd : inout std_logic; waoex : buffer integer; mgf : linkage std_logic_vector(4 to 4));
end ndakh;

library ieee;
use ieee.std_logic_1164.all;

architecture qoxabmw of ndakh is
  signal auvkk : bit_vector(2 to 0);
  signal rd : time_vector(3 downto 2);
  signal xt : bit_vector(2 to 0);
  signal jscbxvnbyc : time_vector(3 downto 2);
  signal hig : bit_vector(2 to 0);
  signal qdt : std_logic_vector(0 downto 3);
  signal jf : time_vector(3 downto 2);
  signal mxypz : bit_vector(2 to 0);
  signal vqomfg : std_logic_vector(0 downto 3);
  signal qignnxxxab : time_vector(3 downto 2);
begin
  hez : entity work.rebz
    port map (ecuoyvr => qignnxxxab, fdgikbwkd => vqomfg, mgxaa => mxypz);
  zcisk : entity work.rebz
    port map (ecuoyvr => jf, fdgikbwkd => qdt, mgxaa => hig);
  dztnqww : entity work.rebz
    port map (ecuoyvr => jscbxvnbyc, fdgikbwkd => qdt, mgxaa => xt);
  jsqgbmnqnk : entity work.rebz
    port map (ecuoyvr => rd, fdgikbwkd => vqomfg, mgxaa => auvkk);
  
  -- Multi-driven assignments
  lusvabd <= 'H';
end qoxabmw;

entity b is
  port (jdh : buffer real);
end b;

library ieee;
use ieee.std_logic_1164.all;

architecture q of b is
  signal hm : bit_vector(2 to 0);
  signal i : std_logic_vector(0 downto 3);
  signal fwtroyst : time_vector(3 downto 2);
  signal xyhn : bit_vector(2 to 0);
  signal bqqbqqc : std_logic_vector(0 downto 3);
  signal jaiw : time_vector(3 downto 2);
begin
  eozqaei : entity work.rebz
    port map (ecuoyvr => jaiw, fdgikbwkd => bqqbqqc, mgxaa => xyhn);
  naagqzn : entity work.rebz
    port map (ecuoyvr => fwtroyst, fdgikbwkd => i, mgxaa => hm);
  
  -- Single-driven assignments
  jdh <= jdh;
end q;



-- Seed after: 9306344062193820217,17234720251424330329
