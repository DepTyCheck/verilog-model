-- Seed: 4224210707655086596,3400751927341804175

entity rhylftpg is
  port (nagcnumg : out time; ymfdwxme : linkage boolean_vector(0 downto 4));
end rhylftpg;

architecture kkvzgzdh of rhylftpg is
  
begin
  -- Single-driven assignments
  nagcnumg <= 2 ms;
end kkvzgzdh;

entity ldwkr is
  port (trslhbcnov : out severity_level; tngcflpokn : in time; gfwoi : inout real);
end ldwkr;

architecture tikhvbc of ldwkr is
  signal nwdcm : boolean_vector(0 downto 4);
  signal srrehsbyf : time;
  signal htmpokbf : boolean_vector(0 downto 4);
  signal cwquzzt : time;
  signal cxh : boolean_vector(0 downto 4);
  signal wajdc : time;
  signal amy : boolean_vector(0 downto 4);
  signal j : time;
begin
  cxzcv : entity work.rhylftpg
    port map (nagcnumg => j, ymfdwxme => amy);
  ruqmfys : entity work.rhylftpg
    port map (nagcnumg => wajdc, ymfdwxme => cxh);
  yod : entity work.rhylftpg
    port map (nagcnumg => cwquzzt, ymfdwxme => htmpokbf);
  jokjcdvu : entity work.rhylftpg
    port map (nagcnumg => srrehsbyf, ymfdwxme => nwdcm);
  
  -- Single-driven assignments
  trslhbcnov <= trslhbcnov;
end tikhvbc;

entity uyp is
  port (oqabwyizhx : in real);
end uyp;

architecture ujcjxnokn of uyp is
  signal fdqajpemo : real;
  signal iw : time;
  signal xo : severity_level;
  signal uzkdaq : boolean_vector(0 downto 4);
  signal kfkkqwo : time;
  signal frmgmdx : boolean_vector(0 downto 4);
  signal pomecek : time;
  signal rhbspt : boolean_vector(0 downto 4);
  signal l : time;
begin
  cvyyxeag : entity work.rhylftpg
    port map (nagcnumg => l, ymfdwxme => rhbspt);
  o : entity work.rhylftpg
    port map (nagcnumg => pomecek, ymfdwxme => frmgmdx);
  qllrdt : entity work.rhylftpg
    port map (nagcnumg => kfkkqwo, ymfdwxme => uzkdaq);
  hk : entity work.ldwkr
    port map (trslhbcnov => xo, tngcflpokn => iw, gfwoi => fdqajpemo);
  
  -- Single-driven assignments
  iw <= l;
end ujcjxnokn;

library ieee;
use ieee.std_logic_1164.all;

entity yrjmj is
  port (v : buffer boolean_vector(3 downto 0); xwdej : in time; tqraj : linkage std_logic_vector(1 downto 3));
end yrjmj;

architecture y of yrjmj is
  signal nq : boolean_vector(0 downto 4);
  signal z : time;
  signal hek : boolean_vector(0 downto 4);
  signal mc : time;
  signal p : real;
  signal jahf : boolean_vector(0 downto 4);
  signal cgk : time;
begin
  f : entity work.rhylftpg
    port map (nagcnumg => cgk, ymfdwxme => jahf);
  enogubrpva : entity work.uyp
    port map (oqabwyizhx => p);
  ouhbk : entity work.rhylftpg
    port map (nagcnumg => mc, ymfdwxme => hek);
  ifwr : entity work.rhylftpg
    port map (nagcnumg => z, ymfdwxme => nq);
  
  -- Single-driven assignments
  v <= v;
  p <= 1_0_4_2.1_4_0;
end y;



-- Seed after: 15789384086600203031,3400751927341804175
