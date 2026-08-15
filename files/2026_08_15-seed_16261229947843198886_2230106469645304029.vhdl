-- Seed: 16261229947843198886,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity knx is
  port (krsqak : inout std_logic; po : out std_logic; lhrxfewpq : in std_logic_vector(1 to 3));
end knx;

architecture grqrjemf of knx is
  
begin
  
end grqrjemf;

entity zkhc is
  port (oeyevzf : out bit; rjgnibuatj : buffer real);
end zkhc;

library ieee;
use ieee.std_logic_1164.all;

architecture jjrsj of zkhc is
  signal lyjyb : std_logic_vector(1 to 3);
  signal xncnh : std_logic;
  signal kwfhqfmyzl : std_logic;
begin
  yj : entity work.knx
    port map (krsqak => kwfhqfmyzl, po => xncnh, lhrxfewpq => lyjyb);
  vemxkmi : entity work.knx
    port map (krsqak => kwfhqfmyzl, po => xncnh, lhrxfewpq => lyjyb);
  galhyyd : entity work.knx
    port map (krsqak => kwfhqfmyzl, po => kwfhqfmyzl, lhrxfewpq => lyjyb);
  
  -- Single-driven assignments
  oeyevzf <= oeyevzf;
  rjgnibuatj <= 2.4_2;
  
  -- Multi-driven assignments
  kwfhqfmyzl <= 'H';
end jjrsj;



-- Seed after: 5784368318271866576,2230106469645304029
