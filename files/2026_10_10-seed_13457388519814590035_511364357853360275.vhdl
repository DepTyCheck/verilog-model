-- Seed: 13457388519814590035,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity wce is
  port (ivalfkh : buffer bit_vector(2 downto 4); xjwhkojdsi : out std_logic);
end wce;

architecture gablwll of wce is
  
begin
  -- Single-driven assignments
  ivalfkh <= (others => '0');
  
  -- Multi-driven assignments
  xjwhkojdsi <= xjwhkojdsi;
  xjwhkojdsi <= xjwhkojdsi;
end gablwll;

entity rjkya is
  port (qvkqkkyqs : buffer integer; jtejaj : out integer);
end rjkya;

library ieee;
use ieee.std_logic_1164.all;

architecture fotwrfd of rjkya is
  signal ea : bit_vector(2 downto 4);
  signal wrdpstihlj : bit_vector(2 downto 4);
  signal lvar : bit_vector(2 downto 4);
  signal icco : std_logic;
  signal egyp : bit_vector(2 downto 4);
begin
  dep : entity work.wce
    port map (ivalfkh => egyp, xjwhkojdsi => icco);
  oisjxsqowy : entity work.wce
    port map (ivalfkh => lvar, xjwhkojdsi => icco);
  qyknzmg : entity work.wce
    port map (ivalfkh => wrdpstihlj, xjwhkojdsi => icco);
  blnwxw : entity work.wce
    port map (ivalfkh => ea, xjwhkojdsi => icco);
  
  -- Single-driven assignments
  jtejaj <= 2#1_0_0#;
  qvkqkkyqs <= 1_1_4_4_4;
  
  -- Multi-driven assignments
  icco <= icco;
  icco <= '0';
  icco <= 'L';
  icco <= icco;
end fotwrfd;

entity qmgshg is
  port (m : buffer time);
end qmgshg;

architecture mktrmmyf of qmgshg is
  signal hjpraqjowc : integer;
  signal akshprigp : integer;
begin
  pzjbfiu : entity work.rjkya
    port map (qvkqkkyqs => akshprigp, jtejaj => hjpraqjowc);
  
  -- Single-driven assignments
  m <= 0 fs;
end mktrmmyf;



-- Seed after: 4286802873529176175,511364357853360275
