-- Seed: 654643048540331057,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity mvjbyersi is
  port (cqmuemip : in std_logic_vector(3 to 3); w : inout boolean_vector(2 to 1); ebwnbo : out std_logic; mt : linkage std_logic_vector(1 to 0));
end mvjbyersi;

architecture swx of mvjbyersi is
  
begin
  -- Single-driven assignments
  w <= (others => TRUE);
  
  -- Multi-driven assignments
  ebwnbo <= ebwnbo;
  ebwnbo <= 'H';
  ebwnbo <= 'X';
  ebwnbo <= '-';
end swx;

library ieee;
use ieee.std_logic_1164.all;

entity uaykuol is
  port (qbzc : buffer time; mdknff : buffer std_logic_vector(0 to 4); fgrwmyxzay : in integer);
end uaykuol;

architecture uwqcdlhpgm of uaykuol is
  
begin
  -- Single-driven assignments
  qbzc <= 4_3_1.0 fs;
  
  -- Multi-driven assignments
  mdknff <= mdknff;
end uwqcdlhpgm;

library ieee;
use ieee.std_logic_1164.all;

entity jxdelnfm is
  port (tncfqcn : buffer std_logic);
end jxdelnfm;

library ieee;
use ieee.std_logic_1164.all;

architecture zsrf of jxdelnfm is
  signal ixt : std_logic;
  signal rtlubn : boolean_vector(2 to 1);
  signal tzf : std_logic_vector(1 to 0);
  signal dxv : boolean_vector(2 to 1);
  signal wqg : std_logic_vector(3 to 3);
  signal jgwfouvs : integer;
  signal s : std_logic_vector(0 to 4);
  signal stsit : time;
begin
  eoosj : entity work.uaykuol
    port map (qbzc => stsit, mdknff => s, fgrwmyxzay => jgwfouvs);
  htkm : entity work.mvjbyersi
    port map (cqmuemip => wqg, w => dxv, ebwnbo => tncfqcn, mt => tzf);
  ojpr : entity work.mvjbyersi
    port map (cqmuemip => wqg, w => rtlubn, ebwnbo => ixt, mt => tzf);
  
  -- Multi-driven assignments
  tncfqcn <= 'H';
  tzf <= "";
  tncfqcn <= 'U';
end zsrf;



-- Seed after: 10594454118985633231,14141408946471626091
