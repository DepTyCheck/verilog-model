-- Seed: 16364368907237196910,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity hgmfeg is
  port (ncdbpegma : out real; novjgvhpf : out boolean_vector(1 downto 3); oyvhdnwe : out std_logic_vector(4 to 0));
end hgmfeg;

architecture uf of hgmfeg is
  
begin
  -- Single-driven assignments
  ncdbpegma <= 3003.023;
  novjgvhpf <= (others => TRUE);
  
  -- Multi-driven assignments
  oyvhdnwe <= "";
  oyvhdnwe <= "";
  oyvhdnwe <= "";
  oyvhdnwe <= (others => '0');
end uf;

entity orxxzwjrek is
  port (jr : inout time; hoc : inout bit; vjichw : linkage integer);
end orxxzwjrek;

library ieee;
use ieee.std_logic_1164.all;

architecture rql of orxxzwjrek is
  signal qtgzz : boolean_vector(1 downto 3);
  signal pvajxr : real;
  signal rxdunmy : boolean_vector(1 downto 3);
  signal lptbgwkz : real;
  signal qmbz : std_logic_vector(4 to 0);
  signal zxymdzw : boolean_vector(1 downto 3);
  signal l : real;
begin
  lm : entity work.hgmfeg
    port map (ncdbpegma => l, novjgvhpf => zxymdzw, oyvhdnwe => qmbz);
  zv : entity work.hgmfeg
    port map (ncdbpegma => lptbgwkz, novjgvhpf => rxdunmy, oyvhdnwe => qmbz);
  ukblf : entity work.hgmfeg
    port map (ncdbpegma => pvajxr, novjgvhpf => qtgzz, oyvhdnwe => qmbz);
  
  -- Single-driven assignments
  hoc <= '1';
  jr <= 16#08E# fs;
end rql;



-- Seed after: 5558959275694031788,17047277710231705797
