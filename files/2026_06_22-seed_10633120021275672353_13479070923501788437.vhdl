-- Seed: 10633120021275672353,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity lnkypkpfnm is
  port (cgnssd : linkage std_logic_vector(4 to 4); eij : inout integer);
end lnkypkpfnm;

architecture yz of lnkypkpfnm is
  
begin
  -- Single-driven assignments
  eij <= 2#0_1#;
end yz;

library ieee;
use ieee.std_logic_1164.all;

entity qamhoeob is
  port (tohmp : linkage std_logic_vector(4 to 2));
end qamhoeob;

library ieee;
use ieee.std_logic_1164.all;

architecture ghvul of qamhoeob is
  signal rmc : integer;
  signal almh : std_logic_vector(4 to 4);
begin
  btaruow : entity work.lnkypkpfnm
    port map (cgnssd => almh, eij => rmc);
  
  -- Multi-driven assignments
  almh <= "W";
  almh <= "1";
  almh <= (others => 'L');
end ghvul;

entity nfnjxht is
  port (pt : in time; mnxej : out integer; iv : buffer time);
end nfnjxht;

library ieee;
use ieee.std_logic_1164.all;

architecture pmuntkp of nfnjxht is
  signal xqszarrgma : std_logic_vector(4 to 2);
  signal txhd : std_logic_vector(4 to 2);
  signal pdkmr : std_logic_vector(4 to 4);
begin
  lifi : entity work.lnkypkpfnm
    port map (cgnssd => pdkmr, eij => mnxej);
  xzempzd : entity work.qamhoeob
    port map (tohmp => txhd);
  nxlmh : entity work.qamhoeob
    port map (tohmp => xqszarrgma);
  
  -- Single-driven assignments
  iv <= 0314.203 ns;
end pmuntkp;



-- Seed after: 1534932415372619450,13479070923501788437
