-- Seed: 16183954218213430247,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity xcesld is
  port (ldd : in std_logic; kzqlfhbf : out std_logic; ycuo : in std_logic_vector(0 to 1));
end xcesld;

architecture uyolhm of xcesld is
  
begin
  -- Multi-driven assignments
  kzqlfhbf <= 'X';
  kzqlfhbf <= 'X';
  kzqlfhbf <= 'L';
end uyolhm;

entity bsrfm is
  port (wfdpohvqg : out real; v : in real);
end bsrfm;

library ieee;
use ieee.std_logic_1164.all;

architecture waqowarskl of bsrfm is
  signal scezfrlehq : std_logic_vector(0 to 1);
  signal nubmbjff : std_logic;
begin
  gtxpm : entity work.xcesld
    port map (ldd => nubmbjff, kzqlfhbf => nubmbjff, ycuo => scezfrlehq);
  
  -- Single-driven assignments
  wfdpohvqg <= 2_4_1_1_4.4;
  
  -- Multi-driven assignments
  scezfrlehq <= "1W";
  scezfrlehq <= "0U";
end waqowarskl;

library ieee;
use ieee.std_logic_1164.all;

entity dpql is
  port (evmer : out std_logic_vector(4 to 0));
end dpql;

library ieee;
use ieee.std_logic_1164.all;

architecture qwgqadmje of dpql is
  signal xue : std_logic_vector(0 to 1);
  signal eiwk : std_logic;
  signal bvqcqcdr : std_logic;
  signal rnczbr : real;
  signal ouuwmcrjh : real;
begin
  zloiw : entity work.bsrfm
    port map (wfdpohvqg => ouuwmcrjh, v => rnczbr);
  reerh : entity work.xcesld
    port map (ldd => bvqcqcdr, kzqlfhbf => eiwk, ycuo => xue);
  gotw : entity work.bsrfm
    port map (wfdpohvqg => rnczbr, v => ouuwmcrjh);
  
  -- Multi-driven assignments
  eiwk <= '-';
  evmer <= (others => '0');
end qwgqadmje;



-- Seed after: 14152754318437798558,17924494779688682807
