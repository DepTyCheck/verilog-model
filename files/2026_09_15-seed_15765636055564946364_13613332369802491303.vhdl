-- Seed: 15765636055564946364,13613332369802491303

entity c is
  port (sm : buffer time);
end c;

architecture uzdkkhzd of c is
  
begin
  -- Single-driven assignments
  sm <= 8#2_0.63# fs;
end uzdkkhzd;

entity kpar is
  port (t : inout real_vector(3 downto 1); s : in integer_vector(4 downto 2));
end kpar;

architecture wvxfjgjhcd of kpar is
  signal bruvwajn : time;
  signal cifxy : time;
  signal olj : time;
  signal lmrhake : time;
begin
  xrmnfkdgwa : entity work.c
    port map (sm => lmrhake);
  wvsrq : entity work.c
    port map (sm => olj);
  zdoccriq : entity work.c
    port map (sm => cifxy);
  wxhjmgv : entity work.c
    port map (sm => bruvwajn);
  
  -- Single-driven assignments
  t <= (2#0000.110#, 8#756.1#, 3.4_1_3);
end wvxfjgjhcd;

library ieee;
use ieee.std_logic_1164.all;

entity zpmaug is
  port (ctpvhdcmo : linkage std_logic_vector(2 to 4); ztq : linkage bit);
end zpmaug;

architecture my of zpmaug is
  signal akeb : time;
  signal hz : time;
  signal mdxtjzitf : integer_vector(4 downto 2);
  signal hqejxwnedl : real_vector(3 downto 1);
  signal mqplayckqs : time;
begin
  eue : entity work.c
    port map (sm => mqplayckqs);
  glfwper : entity work.kpar
    port map (t => hqejxwnedl, s => mdxtjzitf);
  fk : entity work.c
    port map (sm => hz);
  io : entity work.c
    port map (sm => akeb);
  
  -- Single-driven assignments
  mdxtjzitf <= (4_4_4, 3334, 041);
end my;



-- Seed after: 8828626743006649709,13613332369802491303
