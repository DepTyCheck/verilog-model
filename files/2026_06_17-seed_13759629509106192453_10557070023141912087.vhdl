-- Seed: 13759629509106192453,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity jpa is
  port (et : buffer time; gnbdovxw : buffer integer; pwrec : inout std_logic; uu : in std_logic_vector(4 to 0));
end jpa;

architecture m of jpa is
  
begin
  -- Single-driven assignments
  gnbdovxw <= 2#1#;
  et <= 1 sec;
  
  -- Multi-driven assignments
  pwrec <= 'W';
  pwrec <= 'W';
  pwrec <= '-';
end m;

entity tf is
  port (q : inout integer; aeudgqoeoi : in integer; vazzoygy : out integer; nsltmtev : buffer time);
end tf;

library ieee;
use ieee.std_logic_1164.all;

architecture k of tf is
  signal zjbusmoya : std_logic;
  signal z : time;
  signal zxp : std_logic_vector(4 to 0);
  signal put : integer;
  signal ydgojulxw : time;
  signal trwcpkdq : std_logic_vector(4 to 0);
  signal qmqu : std_logic;
  signal xxt : integer;
  signal snycwq : time;
begin
  y : entity work.jpa
    port map (et => snycwq, gnbdovxw => xxt, pwrec => qmqu, uu => trwcpkdq);
  kdof : entity work.jpa
    port map (et => ydgojulxw, gnbdovxw => put, pwrec => qmqu, uu => zxp);
  aqbxobik : entity work.jpa
    port map (et => nsltmtev, gnbdovxw => vazzoygy, pwrec => qmqu, uu => zxp);
  zqfeikwhtn : entity work.jpa
    port map (et => z, gnbdovxw => q, pwrec => zjbusmoya, uu => trwcpkdq);
end k;



-- Seed after: 464003694638319862,10557070023141912087
