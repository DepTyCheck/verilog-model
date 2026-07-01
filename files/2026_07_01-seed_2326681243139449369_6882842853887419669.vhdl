-- Seed: 2326681243139449369,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity ztwkhbudfz is
  port (co : buffer std_logic);
end ztwkhbudfz;

architecture sglo of ztwkhbudfz is
  
begin
  -- Multi-driven assignments
  co <= '0';
  co <= '-';
  co <= 'L';
  co <= '1';
end sglo;

entity md is
  port (gxrvoo : buffer bit_vector(1 to 0); xhyoexn : in integer; das : buffer severity_level; ucmpcxhsrk : linkage time_vector(1 downto 0));
end md;

library ieee;
use ieee.std_logic_1164.all;

architecture pzfaiohl of md is
  signal a : std_logic;
begin
  qbe : entity work.ztwkhbudfz
    port map (co => a);
  
  -- Single-driven assignments
  das <= NOTE;
  gxrvoo <= (others => '0');
  
  -- Multi-driven assignments
  a <= 'L';
  a <= 'Z';
end pzfaiohl;

library ieee;
use ieee.std_logic_1164.all;

entity ccgmmt is
  port (hurykygpew : in integer; xz : linkage time; otkigqtaze : buffer boolean_vector(1 to 3); oa : inout std_logic_vector(3 downto 0));
end ccgmmt;

library ieee;
use ieee.std_logic_1164.all;

architecture uub of ccgmmt is
  signal yq : std_logic;
  signal pzpuoq : time_vector(1 downto 0);
  signal fuoi : severity_level;
  signal srafbvtimf : integer;
  signal jd : bit_vector(1 to 0);
  signal dcsyschand : time_vector(1 downto 0);
  signal clrirpl : severity_level;
  signal ivxglsnjor : integer;
  signal obr : bit_vector(1 to 0);
begin
  ppzewkty : entity work.md
    port map (gxrvoo => obr, xhyoexn => ivxglsnjor, das => clrirpl, ucmpcxhsrk => dcsyschand);
  vz : entity work.md
    port map (gxrvoo => jd, xhyoexn => srafbvtimf, das => fuoi, ucmpcxhsrk => pzpuoq);
  zjfuhusk : entity work.ztwkhbudfz
    port map (co => yq);
  qn : entity work.ztwkhbudfz
    port map (co => yq);
  
  -- Multi-driven assignments
  oa <= "HZZ0";
end uub;



-- Seed after: 2269207516556339530,6882842853887419669
