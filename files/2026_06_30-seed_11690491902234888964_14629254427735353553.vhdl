-- Seed: 11690491902234888964,14629254427735353553

entity lsxycn is
  port (kaicw : in time);
end lsxycn;

architecture jlb of lsxycn is
  
begin
  
end jlb;

library ieee;
use ieee.std_logic_1164.all;

entity cferwye is
  port (zhkffrtwvd : buffer std_logic);
end cferwye;

architecture eyommmbzv of cferwye is
  signal gofpkolv : time;
  signal lzyipst : time;
  signal bosfwusr : time;
begin
  cxuarkqnc : entity work.lsxycn
    port map (kaicw => bosfwusr);
  ghtfnafwj : entity work.lsxycn
    port map (kaicw => lzyipst);
  wlmmp : entity work.lsxycn
    port map (kaicw => gofpkolv);
  hhoffaqyuu : entity work.lsxycn
    port map (kaicw => bosfwusr);
  
  -- Single-driven assignments
  gofpkolv <= 2#0# ps;
  
  -- Multi-driven assignments
  zhkffrtwvd <= '0';
  zhkffrtwvd <= 'W';
end eyommmbzv;

entity ujtkxx is
  port (dv : in time);
end ujtkxx;

architecture jzt of ujtkxx is
  signal avbkaue : time;
begin
  surfzhtu : entity work.lsxycn
    port map (kaicw => dv);
  cdolxhb : entity work.lsxycn
    port map (kaicw => avbkaue);
  gpx : entity work.lsxycn
    port map (kaicw => dv);
  wdphzbnas : entity work.lsxycn
    port map (kaicw => dv);
  
  -- Single-driven assignments
  avbkaue <= 1_4 ps;
end jzt;



-- Seed after: 7493051216108730397,14629254427735353553
