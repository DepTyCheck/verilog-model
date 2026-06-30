-- Seed: 14423746737391892939,14629254427735353553

entity p is
  port (ym : buffer real; zzhw : buffer real; ncnyhtheif : linkage time; i : out severity_level);
end p;

architecture bxixr of p is
  
begin
  -- Single-driven assignments
  i <= FAILURE;
  ym <= 2#1_1_0_0_1.0000#;
  zzhw <= 4342.4_1;
end bxixr;

entity fvmjq is
  port (pexvfm : buffer real; qhl : linkage integer);
end fvmjq;

architecture nrh of fvmjq is
  signal gab : severity_level;
  signal g : time;
  signal qw : real;
  signal glokwjwtyk : real;
  signal uvpc : severity_level;
  signal vllkq : time;
  signal wvrjaku : real;
  signal tkmw : severity_level;
  signal fierb : time;
  signal uebsr : real;
  signal gi : real;
  signal dizplz : severity_level;
  signal flzplpq : time;
  signal irsxscsm : real;
  signal qpy : real;
begin
  hqc : entity work.p
    port map (ym => qpy, zzhw => irsxscsm, ncnyhtheif => flzplpq, i => dizplz);
  se : entity work.p
    port map (ym => gi, zzhw => uebsr, ncnyhtheif => fierb, i => tkmw);
  jy : entity work.p
    port map (ym => wvrjaku, zzhw => pexvfm, ncnyhtheif => vllkq, i => uvpc);
  vfrtb : entity work.p
    port map (ym => glokwjwtyk, zzhw => qw, ncnyhtheif => g, i => gab);
end nrh;

library ieee;
use ieee.std_logic_1164.all;

entity ze is
  port (lbc : in std_logic; vcdiasfwfs : out std_logic_vector(0 downto 3));
end ze;

architecture spzffeg of ze is
  signal mwvalbpjjk : severity_level;
  signal jfwz : time;
  signal wykglf : real;
  signal nztvxvobl : real;
  signal tvoq : integer;
  signal thflzmbi : real;
  signal lgmysg : integer;
  signal liuhomd : real;
  signal gnlfxp : integer;
  signal vxyaxqdz : real;
begin
  tugjrvnbr : entity work.fvmjq
    port map (pexvfm => vxyaxqdz, qhl => gnlfxp);
  ngftg : entity work.fvmjq
    port map (pexvfm => liuhomd, qhl => lgmysg);
  lymjbx : entity work.fvmjq
    port map (pexvfm => thflzmbi, qhl => tvoq);
  rbgm : entity work.p
    port map (ym => nztvxvobl, zzhw => wykglf, ncnyhtheif => jfwz, i => mwvalbpjjk);
  
  -- Multi-driven assignments
  vcdiasfwfs <= (others => '0');
  vcdiasfwfs <= (others => '0');
end spzffeg;



-- Seed after: 6534950238623863404,14629254427735353553
