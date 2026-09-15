-- Seed: 528279833407170185,13613332369802491303

library ieee;
use ieee.std_logic_1164.all;

entity et is
  port (aspg : out real; u : out std_logic; nansazymp : in std_logic);
end et;

architecture lldvwwgj of et is
  
begin
  -- Multi-driven assignments
  u <= 'H';
  u <= 'X';
  u <= 'H';
end lldvwwgj;

entity vgkuhkxdm is
  port (rqtfropgdv : buffer time);
end vgkuhkxdm;

library ieee;
use ieee.std_logic_1164.all;

architecture wofgtrj of vgkuhkxdm is
  signal dfka : real;
  signal vqbnc : real;
  signal u : real;
  signal ukiwmrjvqf : std_logic;
  signal gvwttmdsf : std_logic;
  signal ygmoj : real;
begin
  bgujobzjs : entity work.et
    port map (aspg => ygmoj, u => gvwttmdsf, nansazymp => ukiwmrjvqf);
  mbicodz : entity work.et
    port map (aspg => u, u => gvwttmdsf, nansazymp => ukiwmrjvqf);
  xlsnyu : entity work.et
    port map (aspg => vqbnc, u => gvwttmdsf, nansazymp => gvwttmdsf);
  gvaaqtv : entity work.et
    port map (aspg => dfka, u => gvwttmdsf, nansazymp => gvwttmdsf);
  
  -- Single-driven assignments
  rqtfropgdv <= rqtfropgdv;
  
  -- Multi-driven assignments
  gvwttmdsf <= 'X';
  ukiwmrjvqf <= gvwttmdsf;
  gvwttmdsf <= 'H';
end wofgtrj;

entity gwyvqrmvfg is
  port (weyqsrncn : in time_vector(3 downto 3); nplzsasj : inout real);
end gwyvqrmvfg;

library ieee;
use ieee.std_logic_1164.all;

architecture zldykslvq of gwyvqrmvfg is
  signal qo : std_logic;
  signal jwrm : std_logic;
  signal ehnrn : real;
begin
  u : entity work.et
    port map (aspg => ehnrn, u => jwrm, nansazymp => jwrm);
  xqjhd : entity work.et
    port map (aspg => nplzsasj, u => qo, nansazymp => jwrm);
  
  -- Multi-driven assignments
  jwrm <= jwrm;
  jwrm <= '-';
  qo <= '1';
end zldykslvq;

entity nornacmm is
  port (ium : in integer; zayomapypd : inout boolean; hcvn : out character);
end nornacmm;

architecture kbeltezabs of nornacmm is
  
begin
  -- Single-driven assignments
  hcvn <= 'i';
  zayomapypd <= TRUE;
end kbeltezabs;



-- Seed after: 18420382503576219274,13613332369802491303
