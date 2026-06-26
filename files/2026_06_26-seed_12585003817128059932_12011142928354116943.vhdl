-- Seed: 12585003817128059932,12011142928354116943

entity ueqselyjt is
  port (wg : in time; bbsvletqk : out real);
end ueqselyjt;

architecture gjgdlemxrg of ueqselyjt is
  
begin
  -- Single-driven assignments
  bbsvletqk <= 2#00100.00100#;
end gjgdlemxrg;

library ieee;
use ieee.std_logic_1164.all;

entity zyucui is
  port (mogg : out boolean; qxxlsfd : out integer; em : in std_logic_vector(3 to 4); gdyg : buffer real);
end zyucui;

architecture vyvhq of zyucui is
  signal vaxpongp : time;
  signal guelsbqe : real;
  signal euumwcwr : time;
begin
  vk : entity work.ueqselyjt
    port map (wg => euumwcwr, bbsvletqk => guelsbqe);
  vgsghutmul : entity work.ueqselyjt
    port map (wg => vaxpongp, bbsvletqk => gdyg);
  
  -- Single-driven assignments
  qxxlsfd <= 2#1_1_1#;
end vyvhq;

entity pxckfaxyiy is
  port (cdokk : out time);
end pxckfaxyiy;

library ieee;
use ieee.std_logic_1164.all;

architecture vgjgnw of pxckfaxyiy is
  signal bbwimpw : real;
  signal htqnwz : time;
  signal l : real;
  signal bjb : std_logic_vector(3 to 4);
  signal fxs : integer;
  signal nzxxh : boolean;
begin
  o : entity work.zyucui
    port map (mogg => nzxxh, qxxlsfd => fxs, em => bjb, gdyg => l);
  dbofgnxrsz : entity work.ueqselyjt
    port map (wg => htqnwz, bbsvletqk => bbwimpw);
  
  -- Single-driven assignments
  cdokk <= 0_1_0_1 us;
  
  -- Multi-driven assignments
  bjb <= ('X', 'X');
  bjb <= ('0', 'X');
  bjb <= "XW";
  bjb <= "1L";
end vgjgnw;



-- Seed after: 2346181245045626249,12011142928354116943
