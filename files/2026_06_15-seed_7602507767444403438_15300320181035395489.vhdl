-- Seed: 7602507767444403438,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity qd is
  port (vseay : linkage std_logic);
end qd;

architecture xdztq of qd is
  
begin
  
end xdztq;

library ieee;
use ieee.std_logic_1164.all;

entity bubyv is
  port (fdsutouac : in integer; alfxzuy : buffer std_logic; smigz : in string(3 to 5));
end bubyv;

architecture huigtwe of bubyv is
  
begin
  hnczqsdid : entity work.qd
    port map (vseay => alfxzuy);
  
  -- Multi-driven assignments
  alfxzuy <= 'H';
  alfxzuy <= 'Z';
  alfxzuy <= 'W';
end huigtwe;

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (ajcc : in severity_level; dgxgmzqdi : out std_logic_vector(0 downto 4); lvmvv : inout integer);
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture zfqydd of n is
  signal z : string(3 to 5);
  signal txsyl : integer;
  signal lchfsun : std_logic;
begin
  jtgcocd : entity work.qd
    port map (vseay => lchfsun);
  zcwdhqrz : entity work.bubyv
    port map (fdsutouac => txsyl, alfxzuy => lchfsun, smigz => z);
  
  -- Single-driven assignments
  lvmvv <= 2#11#;
  z <= ('t', 'm', 'x');
  txsyl <= 0;
  
  -- Multi-driven assignments
  lchfsun <= 'W';
  dgxgmzqdi <= "";
  dgxgmzqdi <= "";
  dgxgmzqdi <= (others => '0');
end zfqydd;



-- Seed after: 17585152854989468884,15300320181035395489
