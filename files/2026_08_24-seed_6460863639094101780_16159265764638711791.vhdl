-- Seed: 6460863639094101780,16159265764638711791

entity hxgtvp is
  port (yi : buffer time);
end hxgtvp;

architecture t of hxgtvp is
  
begin
  -- Single-driven assignments
  yi <= yi;
end t;

entity iveszkawu is
  port (zeecth : out time_vector(1 to 4));
end iveszkawu;

architecture gfnubrlywa of iveszkawu is
  signal euhasevxs : time;
  signal cgpq : time;
  signal lipeh : time;
begin
  jer : entity work.hxgtvp
    port map (yi => lipeh);
  zqocls : entity work.hxgtvp
    port map (yi => cgpq);
  lkgtdous : entity work.hxgtvp
    port map (yi => euhasevxs);
  
  -- Single-driven assignments
  zeecth <= (16#2.7# us, 0 sec, 2#0# ps, 8#1_5_5.21# us);
end gfnubrlywa;



-- Seed after: 9047188233669367544,16159265764638711791
