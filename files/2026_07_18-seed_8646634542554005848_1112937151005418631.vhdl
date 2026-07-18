-- Seed: 8646634542554005848,1112937151005418631

entity xiamjmd is
  port (tkxlfu : in real; vqxdrh : inout integer);
end xiamjmd;

architecture vwsnbyc of xiamjmd is
  
begin
  -- Single-driven assignments
  vqxdrh <= vqxdrh;
end vwsnbyc;

library ieee;
use ieee.std_logic_1164.all;

entity fspcp is
  port (rsijr : in std_logic; vlv : out std_logic; g : inout std_logic);
end fspcp;

architecture vehulxus of fspcp is
  
begin
  -- Multi-driven assignments
  vlv <= 'W';
  g <= 'L';
  g <= vlv;
end vehulxus;

entity wsuk is
  port (xcnwhupy : buffer integer_vector(1 downto 3));
end wsuk;

library ieee;
use ieee.std_logic_1164.all;

architecture apnolwu of wsuk is
  signal b : integer;
  signal lzfx : real;
  signal kblnfjxabs : std_logic;
  signal cdnkx : std_logic;
  signal rkjubj : std_logic;
begin
  fjvn : entity work.fspcp
    port map (rsijr => rkjubj, vlv => rkjubj, g => rkjubj);
  s : entity work.fspcp
    port map (rsijr => cdnkx, vlv => cdnkx, g => kblnfjxabs);
  ozlcp : entity work.xiamjmd
    port map (tkxlfu => lzfx, vqxdrh => b);
  
  -- Single-driven assignments
  xcnwhupy <= (others => 0);
  lzfx <= 2_0.1_1_1_3;
  
  -- Multi-driven assignments
  kblnfjxabs <= 'Z';
  kblnfjxabs <= rkjubj;
  rkjubj <= rkjubj;
  rkjubj <= rkjubj;
end apnolwu;



-- Seed after: 12784168828019729716,1112937151005418631
