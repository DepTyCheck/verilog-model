-- Seed: 3616057254697853029,4404421571376382767

entity sfgbzmjy is
  port (rjntal : out time; yoa : out time; gkycmjkjzm : out time; wls : out integer);
end sfgbzmjy;

architecture pjgvsjn of sfgbzmjy is
  
begin
  -- Single-driven assignments
  gkycmjkjzm <= 200.1423 ns;
  yoa <= yoa;
  rjntal <= rjntal;
  wls <= 8#7_0_0_0_7#;
end pjgvsjn;

entity dvjgbaopen is
  port (ibgc : inout real);
end dvjgbaopen;

architecture fqjymzgeki of dvjgbaopen is
  signal m : integer;
  signal vmz : time;
  signal tzch : time;
  signal o : time;
begin
  zunlsylvfx : entity work.sfgbzmjy
    port map (rjntal => o, yoa => tzch, gkycmjkjzm => vmz, wls => m);
end fqjymzgeki;

library ieee;
use ieee.std_logic_1164.all;

entity jo is
  port (uewbwchvf : out std_logic_vector(2 downto 1); hlnbbb : inout integer);
end jo;

architecture jjivoho of jo is
  signal tms : integer;
  signal uvsoakz : time;
  signal ocihpai : time;
  signal d : time;
  signal odzbjjkeva : real;
begin
  xup : entity work.dvjgbaopen
    port map (ibgc => odzbjjkeva);
  sevupad : entity work.sfgbzmjy
    port map (rjntal => d, yoa => ocihpai, gkycmjkjzm => uvsoakz, wls => tms);
  
  -- Single-driven assignments
  hlnbbb <= 1401;
  
  -- Multi-driven assignments
  uewbwchvf <= "-X";
end jjivoho;



-- Seed after: 9483618839466842712,4404421571376382767
