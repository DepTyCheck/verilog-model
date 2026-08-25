-- Seed: 8382385170478447988,13501862637168280927

entity wcuzwtpnbp is
  port (pixdxvg : inout time);
end wcuzwtpnbp;

architecture rhq of wcuzwtpnbp is
  
begin
  
end rhq;

library ieee;
use ieee.std_logic_1164.all;

entity mvjkfp is
  port (wbrpa : buffer time; i : linkage std_logic);
end mvjkfp;

architecture ulsyda of mvjkfp is
  
begin
  wbapv : entity work.wcuzwtpnbp
    port map (pixdxvg => wbrpa);
end ulsyda;

library ieee;
use ieee.std_logic_1164.all;

entity zvtszosmfk is
  port (urtsfd : linkage severity_level; yzo : out time; ttwh : linkage std_logic);
end zvtszosmfk;

architecture jkjq of zvtszosmfk is
  signal avufff : time;
  signal rkaw : time;
begin
  hckjvaoyf : entity work.mvjkfp
    port map (wbrpa => rkaw, i => ttwh);
  fk : entity work.wcuzwtpnbp
    port map (pixdxvg => avufff);
  qdqqbn : entity work.wcuzwtpnbp
    port map (pixdxvg => yzo);
end jkjq;



-- Seed after: 13326800960818449468,13501862637168280927
