-- Seed: 5320575143401700319,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity esxoxe is
  port (osf : linkage integer; ubq : out std_logic; whkpok : linkage time);
end esxoxe;

architecture lfwmpzx of esxoxe is
  
begin
  -- Multi-driven assignments
  ubq <= 'Z';
  ubq <= '0';
  ubq <= '-';
end lfwmpzx;

entity jrwqemxdd is
  port (wdbhbdqb : inout bit);
end jrwqemxdd;

architecture z of jrwqemxdd is
  
begin
  -- Single-driven assignments
  wdbhbdqb <= '0';
end z;

library ieee;
use ieee.std_logic_1164.all;

entity xjihzsga is
  port (uxoqgcwb : linkage integer_vector(0 downto 4); tmvfo : buffer std_logic);
end xjihzsga;

architecture vzxqldxnq of xjihzsga is
  signal ieui : time;
  signal g : integer;
begin
  gbaqx : entity work.esxoxe
    port map (osf => g, ubq => tmvfo, whkpok => ieui);
  
  -- Multi-driven assignments
  tmvfo <= '-';
  tmvfo <= '-';
  tmvfo <= '0';
end vzxqldxnq;

entity xgyo is
  port (pgj : buffer time);
end xgyo;

library ieee;
use ieee.std_logic_1164.all;

architecture glhvch of xgyo is
  signal kxrpkeq : integer;
  signal jkxqytfyl : time;
  signal d : std_logic;
  signal bldskj : integer;
  signal fx : time;
  signal ai : integer;
  signal xmgvjiz : std_logic;
  signal tkatmwzpyh : integer_vector(0 downto 4);
begin
  ntrzoqrh : entity work.xjihzsga
    port map (uxoqgcwb => tkatmwzpyh, tmvfo => xmgvjiz);
  o : entity work.esxoxe
    port map (osf => ai, ubq => xmgvjiz, whkpok => fx);
  jiagy : entity work.esxoxe
    port map (osf => bldskj, ubq => d, whkpok => jkxqytfyl);
  wy : entity work.esxoxe
    port map (osf => kxrpkeq, ubq => xmgvjiz, whkpok => pgj);
  
  -- Multi-driven assignments
  xmgvjiz <= d;
  d <= '-';
  d <= 'L';
  xmgvjiz <= xmgvjiz;
end glhvch;



-- Seed after: 378109759783155566,8067602802092121131
