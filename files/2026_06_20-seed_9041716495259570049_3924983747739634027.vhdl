-- Seed: 9041716495259570049,3924983747739634027

entity sttayqeb is
  port (suhxhjl : linkage boolean; phy : in real; fhkksvxbz : out time; xndqtuyzk : inout time);
end sttayqeb;

architecture pifpbo of sttayqeb is
  
begin
  -- Single-driven assignments
  xndqtuyzk <= 8#35473# ps;
  fhkksvxbz <= 8#2_5_1.746# fs;
end pifpbo;

library ieee;
use ieee.std_logic_1164.all;

entity lwwjubeq is
  port (fllboak : buffer time; gttivv : buffer std_logic; rhxadpoy : buffer real);
end lwwjubeq;

architecture akjqds of lwwjubeq is
  signal i : time;
  signal xrbbsxou : time;
  signal yllfgrwyi : boolean;
begin
  zzkw : entity work.sttayqeb
    port map (suhxhjl => yllfgrwyi, phy => rhxadpoy, fhkksvxbz => xrbbsxou, xndqtuyzk => i);
end akjqds;



-- Seed after: 14216502808589295537,3924983747739634027
