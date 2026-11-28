-- Seed: 4903051454359083115,15306146742274979323

entity ykbkdbzsng is
  port (xapotuknud : buffer time; dcrxl : in time);
end ykbkdbzsng;

architecture ru of ykbkdbzsng is
  
begin
  -- Single-driven assignments
  xapotuknud <= 0 ps;
end ru;

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (feijtjopz : linkage real; z : in std_logic; jacrmexfe : inout time);
end p;

architecture bjwo of p is
  signal syxlu : time;
  signal dikfvbiui : time;
  signal vcvstnpz : time;
  signal hssyprrp : time;
begin
  eirdo : entity work.ykbkdbzsng
    port map (xapotuknud => hssyprrp, dcrxl => vcvstnpz);
  qzmrnnspzg : entity work.ykbkdbzsng
    port map (xapotuknud => vcvstnpz, dcrxl => dikfvbiui);
  rirzqsxtv : entity work.ykbkdbzsng
    port map (xapotuknud => syxlu, dcrxl => jacrmexfe);
  nycqccaaat : entity work.ykbkdbzsng
    port map (xapotuknud => jacrmexfe, dcrxl => jacrmexfe);
  
  -- Single-driven assignments
  dikfvbiui <= jacrmexfe;
end bjwo;



-- Seed after: 8987814963241261775,15306146742274979323
