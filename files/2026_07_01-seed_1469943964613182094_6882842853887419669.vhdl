-- Seed: 1469943964613182094,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity q is
  port (jwp : in std_logic; mvtmqy : inout integer; ioponozh : linkage real);
end q;

architecture ciuk of q is
  
begin
  -- Single-driven assignments
  mvtmqy <= 8#43#;
end ciuk;

library ieee;
use ieee.std_logic_1164.all;

entity da is
  port (nnynxcemma : linkage std_logic; y : out severity_level; xlhusgqkh : out bit_vector(1 downto 2));
end da;

library ieee;
use ieee.std_logic_1164.all;

architecture ajerny of da is
  signal cpsvxy : real;
  signal rnfkwllvw : integer;
  signal cxihyzk : real;
  signal vs : integer;
  signal ihnisvcv : real;
  signal mufpc : integer;
  signal ipt : std_logic;
  signal f : real;
  signal nlbsq : integer;
  signal hen : std_logic;
begin
  ehqtxhs : entity work.q
    port map (jwp => hen, mvtmqy => nlbsq, ioponozh => f);
  qfq : entity work.q
    port map (jwp => ipt, mvtmqy => mufpc, ioponozh => ihnisvcv);
  mavgs : entity work.q
    port map (jwp => ipt, mvtmqy => vs, ioponozh => cxihyzk);
  qioziqn : entity work.q
    port map (jwp => hen, mvtmqy => rnfkwllvw, ioponozh => cpsvxy);
  
  -- Single-driven assignments
  xlhusgqkh <= (others => '0');
  
  -- Multi-driven assignments
  hen <= '-';
  hen <= 'Z';
  hen <= 'X';
end ajerny;

library ieee;
use ieee.std_logic_1164.all;

entity px is
  port (rfcxqftu : inout string(1 to 3); kvnj : out std_logic_vector(3 downto 1); wrz : buffer std_logic; n : linkage std_logic_vector(0 to 3));
end px;

architecture beff of px is
  signal qyhiyqsih : real;
  signal ctmoehwh : integer;
begin
  kjqbuxetpz : entity work.q
    port map (jwp => wrz, mvtmqy => ctmoehwh, ioponozh => qyhiyqsih);
  
  -- Single-driven assignments
  rfcxqftu <= "ykt";
  
  -- Multi-driven assignments
  kvnj <= "ZU1";
  wrz <= '-';
end beff;



-- Seed after: 6485331693488326332,6882842853887419669
