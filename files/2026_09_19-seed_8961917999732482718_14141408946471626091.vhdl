-- Seed: 8961917999732482718,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity urqdnw is
  port (gvog : inout time; luteguxwwr : buffer std_logic_vector(4 downto 3); wlxloprq : buffer std_logic);
end urqdnw;

architecture tjz of urqdnw is
  
begin
  -- Single-driven assignments
  gvog <= gvog;
  
  -- Multi-driven assignments
  wlxloprq <= 'W';
  wlxloprq <= 'Z';
  wlxloprq <= 'W';
  wlxloprq <= '-';
end tjz;

entity orwcwltfp is
  port (xksqnjh : buffer bit_vector(0 downto 0));
end orwcwltfp;

library ieee;
use ieee.std_logic_1164.all;

architecture imgzmhuj of orwcwltfp is
  signal cejdqgghq : std_logic_vector(4 downto 3);
  signal wabtgujj : time;
  signal zv : std_logic;
  signal lyzn : time;
  signal ikskaxeb : time;
  signal n : std_logic;
  signal v : std_logic_vector(4 downto 3);
  signal xphxzj : time;
begin
  ybfzla : entity work.urqdnw
    port map (gvog => xphxzj, luteguxwwr => v, wlxloprq => n);
  iryhdajq : entity work.urqdnw
    port map (gvog => ikskaxeb, luteguxwwr => v, wlxloprq => n);
  boazcpv : entity work.urqdnw
    port map (gvog => lyzn, luteguxwwr => v, wlxloprq => zv);
  hag : entity work.urqdnw
    port map (gvog => wabtgujj, luteguxwwr => cejdqgghq, wlxloprq => n);
  
  -- Single-driven assignments
  xksqnjh <= (others => '1');
end imgzmhuj;



-- Seed after: 14927309991516652255,14141408946471626091
