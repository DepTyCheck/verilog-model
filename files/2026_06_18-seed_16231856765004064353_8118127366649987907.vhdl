-- Seed: 16231856765004064353,8118127366649987907

entity dfjdpe is
  port (ulj : buffer time; pkvvgsxb : out real);
end dfjdpe;

architecture clxiccitu of dfjdpe is
  
begin
  -- Single-driven assignments
  ulj <= 2#111.0# ms;
  pkvvgsxb <= 2#11011.01#;
end clxiccitu;

entity rv is
  port (t : out real);
end rv;

architecture lfux of rv is
  signal sjexwzwipa : time;
  signal yotkumbj : real;
  signal pqqbbale : time;
  signal korsl : real;
  signal fskcepkxpz : time;
begin
  a : entity work.dfjdpe
    port map (ulj => fskcepkxpz, pkvvgsxb => korsl);
  hr : entity work.dfjdpe
    port map (ulj => pqqbbale, pkvvgsxb => yotkumbj);
  ugwgmszcy : entity work.dfjdpe
    port map (ulj => sjexwzwipa, pkvvgsxb => t);
end lfux;



-- Seed after: 10361640498566637571,8118127366649987907
