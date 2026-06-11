-- Seed: 8197375934313249038,11181851762153539145



entity jocfj is
  port (xlpxyit : buffer time);
end jocfj;



architecture zobtwwoacu of jocfj is
  
begin
  
end zobtwwoacu;



entity bowexmu is
  port (ilmcy : out bit_vector(0 downto 2); onflvffvi : in boolean);
end bowexmu;



architecture qzfvi of bowexmu is
  signal wjdhbbta : time;
begin
  efna : entity work.jocfj
    port map (xlpxyit => wjdhbbta);
end qzfvi;

library ieee;
use ieee.std_logic_1164.all;

entity hzjcx is
  port (lvqfqu : out std_logic; uqdmf : in std_logic_vector(1 to 4); aam : in time_vector(3 downto 1); hhzhh : inout integer);
end hzjcx;



architecture ibtpdsuppj of hzjcx is
  signal owhxjxljl : boolean;
  signal flkxypjsxd : bit_vector(0 downto 2);
  signal zgtzxrd : boolean;
  signal fptpe : bit_vector(0 downto 2);
  signal cxazmndkdr : time;
  signal yxnqnife : time;
begin
  mq : entity work.jocfj
    port map (xlpxyit => yxnqnife);
  qksyvxgbtx : entity work.jocfj
    port map (xlpxyit => cxazmndkdr);
  z : entity work.bowexmu
    port map (ilmcy => fptpe, onflvffvi => zgtzxrd);
  ntjyw : entity work.bowexmu
    port map (ilmcy => flkxypjsxd, onflvffvi => owhxjxljl);
end ibtpdsuppj;



-- Seed after: 6419712288517553923,11181851762153539145
