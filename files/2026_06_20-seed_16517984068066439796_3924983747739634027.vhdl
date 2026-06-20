-- Seed: 16517984068066439796,3924983747739634027

entity thicvdlv is
  port (qdvi : out real_vector(4 to 0); tiftg : in real);
end thicvdlv;

architecture scomycm of thicvdlv is
  
begin
  -- Single-driven assignments
  qdvi <= (others => 0.0);
end scomycm;

library ieee;
use ieee.std_logic_1164.all;

entity mcst is
  port (qqd : buffer std_logic; y : inout character; ka : buffer std_logic_vector(1 to 4); cqrlecf : inout std_logic_vector(3 to 0));
end mcst;

architecture faclntm of mcst is
  
begin
  -- Single-driven assignments
  y <= 'o';
end faclntm;

library ieee;
use ieee.std_logic_1164.all;

entity uthmcsd is
  port (rik : in bit; trfc : buffer real; jwwyovyqg : in std_logic_vector(4 downto 4); nvazge : buffer time);
end uthmcsd;

architecture fz of uthmcsd is
  signal sp : real;
  signal xoqn : real_vector(4 to 0);
begin
  cnknayr : entity work.thicvdlv
    port map (qdvi => xoqn, tiftg => sp);
end fz;

entity vwrfzpx is
  port (snfplofg : inout integer; gcajnin : in real_vector(1 downto 3));
end vwrfzpx;

library ieee;
use ieee.std_logic_1164.all;

architecture opflz of vwrfzpx is
  signal ftz : time;
  signal k : std_logic_vector(4 downto 4);
  signal g : bit;
  signal whr : real;
  signal imas : real_vector(4 to 0);
  signal wgffox : real_vector(4 to 0);
  signal iqrmq : real;
  signal bhvqdcou : real_vector(4 to 0);
begin
  al : entity work.thicvdlv
    port map (qdvi => bhvqdcou, tiftg => iqrmq);
  rbebno : entity work.thicvdlv
    port map (qdvi => wgffox, tiftg => iqrmq);
  ajwerlw : entity work.thicvdlv
    port map (qdvi => imas, tiftg => whr);
  jbnrdqlyc : entity work.uthmcsd
    port map (rik => g, trfc => whr, jwwyovyqg => k, nvazge => ftz);
  
  -- Single-driven assignments
  snfplofg <= 204;
  
  -- Multi-driven assignments
  k <= "U";
  k <= "0";
  k <= (others => '-');
end opflz;



-- Seed after: 16260863137089682844,3924983747739634027
