-- Seed: 13152532439100574272,13613332369802491303

library ieee;
use ieee.std_logic_1164.all;

entity zqtkuffnfv is
  port (gpqdxcb : inout std_logic_vector(0 to 1); lxxlssk : inout time);
end zqtkuffnfv;

architecture wtnqmhqzrj of zqtkuffnfv is
  
begin
  -- Single-driven assignments
  lxxlssk <= 2 sec;
  
  -- Multi-driven assignments
  gpqdxcb <= "01";
  gpqdxcb <= gpqdxcb;
  gpqdxcb <= gpqdxcb;
end wtnqmhqzrj;

entity f is
  port (mtkduyfbof : inout time; hemtqmnpnr : out time);
end f;

architecture ouxjcyjxa of f is
  
begin
  
end ouxjcyjxa;

entity rqsbrlomun is
  port (fbjihwu : in severity_level; qyvmmbzux : linkage string(4 to 2); qnm : inout time; psxvuwcnxz : in boolean);
end rqsbrlomun;

library ieee;
use ieee.std_logic_1164.all;

architecture jszefwngml of rqsbrlomun is
  signal titghycznp : time;
  signal acr : time;
  signal qpelldxwz : time;
  signal xjpcuu : std_logic_vector(0 to 1);
  signal ekwaifxn : time;
  signal tdmjcijpcb : time;
begin
  z : entity work.f
    port map (mtkduyfbof => tdmjcijpcb, hemtqmnpnr => ekwaifxn);
  xdpkvyyy : entity work.zqtkuffnfv
    port map (gpqdxcb => xjpcuu, lxxlssk => qpelldxwz);
  d : entity work.f
    port map (mtkduyfbof => acr, hemtqmnpnr => titghycznp);
  auc : entity work.zqtkuffnfv
    port map (gpqdxcb => xjpcuu, lxxlssk => qnm);
  
  -- Multi-driven assignments
  xjpcuu <= ('U', '-');
  xjpcuu <= ('-', 'X');
  xjpcuu <= xjpcuu;
end jszefwngml;



-- Seed after: 3741357989361067577,13613332369802491303
