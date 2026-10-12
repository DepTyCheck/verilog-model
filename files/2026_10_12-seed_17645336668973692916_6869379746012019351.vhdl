-- Seed: 17645336668973692916,6869379746012019351

library ieee;
use ieee.std_logic_1164.all;

entity vn is
  port (utzlxvvj : buffer std_logic_vector(3 downto 3); mtnk : in time);
end vn;

architecture lxclo of vn is
  
begin
  
end lxclo;

library ieee;
use ieee.std_logic_1164.all;

entity nwrvhfbsn is
  port (vnozdpe : buffer integer; lhb : inout std_logic_vector(2 downto 3); qrcmeetudc : out time);
end nwrvhfbsn;

library ieee;
use ieee.std_logic_1164.all;

architecture uewixnk of nwrvhfbsn is
  signal efqqlbx : time;
  signal yvu : std_logic_vector(3 downto 3);
  signal tdkgzeyjmn : time;
  signal vlu : std_logic_vector(3 downto 3);
  signal hcjhmu : std_logic_vector(3 downto 3);
  signal gshqoq : time;
  signal fiejfg : std_logic_vector(3 downto 3);
begin
  xupr : entity work.vn
    port map (utzlxvvj => fiejfg, mtnk => gshqoq);
  wceth : entity work.vn
    port map (utzlxvvj => hcjhmu, mtnk => qrcmeetudc);
  gzkhprg : entity work.vn
    port map (utzlxvvj => vlu, mtnk => tdkgzeyjmn);
  qkqo : entity work.vn
    port map (utzlxvvj => yvu, mtnk => efqqlbx);
  
  -- Single-driven assignments
  qrcmeetudc <= qrcmeetudc;
  tdkgzeyjmn <= qrcmeetudc;
  gshqoq <= tdkgzeyjmn;
  efqqlbx <= gshqoq;
  vnozdpe <= 2#0_1_1_1#;
  
  -- Multi-driven assignments
  lhb <= (others => '0');
  lhb <= (others => '0');
end uewixnk;

entity gbgtzwqd is
  port (firy : linkage severity_level);
end gbgtzwqd;

library ieee;
use ieee.std_logic_1164.all;

architecture ghuaze of gbgtzwqd is
  signal obstoozse : time;
  signal cta : std_logic_vector(2 downto 3);
  signal fg : integer;
begin
  lgweotpg : entity work.nwrvhfbsn
    port map (vnozdpe => fg, lhb => cta, qrcmeetudc => obstoozse);
  
  -- Multi-driven assignments
  cta <= "";
  cta <= cta;
  cta <= "";
  cta <= cta;
end ghuaze;

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (en : inout std_logic);
end m;

library ieee;
use ieee.std_logic_1164.all;

architecture bwrqvmgub of m is
  signal bnregbfs : time;
  signal evuyhpeyp : std_logic_vector(3 downto 3);
begin
  cjjq : entity work.vn
    port map (utzlxvvj => evuyhpeyp, mtnk => bnregbfs);
  
  -- Single-driven assignments
  bnregbfs <= bnregbfs;
  
  -- Multi-driven assignments
  en <= en;
  en <= 'U';
  en <= en;
end bwrqvmgub;



-- Seed after: 3375559782805554979,6869379746012019351
