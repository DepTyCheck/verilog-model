-- Seed: 7584314541974131331,8089241273282434469

library ieee;
use ieee.std_logic_1164.all;

entity elslm is
  port (fdhmoddy : in real; xaisavbfvu : buffer std_logic_vector(0 to 3); ogxub : out time_vector(2 to 2));
end elslm;



architecture agtdlpwsv of elslm is
  
begin
  
end agtdlpwsv;



entity h is
  port (ippghu : inout real; zdqlpz : inout real; nctuk : in real_vector(1 downto 2); uejiwwjpad : out real_vector(2 to 0));
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture lrjmemcnt of h is
  signal jktlyutwnn : time_vector(2 to 2);
  signal hydnbaq : std_logic_vector(0 to 3);
  signal qjkeo : real;
begin
  r : entity work.elslm
    port map (fdhmoddy => qjkeo, xaisavbfvu => hydnbaq, ogxub => jktlyutwnn);
end lrjmemcnt;



entity ypmxl is
  port (roguqiki : out integer_vector(2 to 1); ntphnr : out integer; auuufozn : buffer integer);
end ypmxl;

library ieee;
use ieee.std_logic_1164.all;

architecture qlp of ypmxl is
  signal foxpd : time_vector(2 to 2);
  signal tc : real;
  signal lzcvekr : time_vector(2 to 2);
  signal zijxeche : std_logic_vector(0 to 3);
  signal zakmjeqxm : real;
begin
  zyxfdel : entity work.elslm
    port map (fdhmoddy => zakmjeqxm, xaisavbfvu => zijxeche, ogxub => lzcvekr);
  phqudeq : entity work.elslm
    port map (fdhmoddy => tc, xaisavbfvu => zijxeche, ogxub => foxpd);
end qlp;



-- Seed after: 10520279375166925257,8089241273282434469
