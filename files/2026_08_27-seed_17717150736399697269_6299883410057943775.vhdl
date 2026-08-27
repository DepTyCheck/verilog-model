-- Seed: 17717150736399697269,6299883410057943775

entity yjs is
  port (ahwjmp : in integer; zjgjbku : inout integer; atpuip : in time);
end yjs;

architecture p of yjs is
  
begin
  
end p;

entity bawh is
  port (aceq : linkage integer; lu : in integer);
end bawh;

architecture jioyzz of bawh is
  signal q : integer;
  signal yng : integer;
  signal htjhzvndf : time;
  signal vkporf : integer;
  signal fahxrhc : integer;
begin
  hczye : entity work.yjs
    port map (ahwjmp => fahxrhc, zjgjbku => vkporf, atpuip => htjhzvndf);
  fdjadrhtm : entity work.yjs
    port map (ahwjmp => yng, zjgjbku => q, atpuip => htjhzvndf);
  
  -- Single-driven assignments
  yng <= 8#0#;
  htjhzvndf <= htjhzvndf;
  fahxrhc <= lu;
end jioyzz;

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (ofjyhewqs : buffer time; j : inout std_logic_vector(2 downto 3));
end p;

architecture jwpj of p is
  signal tzqx : time;
  signal obmqfxihbf : integer;
  signal dntj : integer;
  signal iojhwiruph : integer;
begin
  uuy : entity work.bawh
    port map (aceq => iojhwiruph, lu => iojhwiruph);
  aq : entity work.yjs
    port map (ahwjmp => dntj, zjgjbku => obmqfxihbf, atpuip => ofjyhewqs);
  gsurrneu : entity work.yjs
    port map (ahwjmp => iojhwiruph, zjgjbku => dntj, atpuip => tzqx);
  
  -- Multi-driven assignments
  j <= j;
  j <= (others => '0');
  j <= j;
  j <= "";
end jwpj;

entity yioucneoq is
  port (ecqf : out time);
end yioucneoq;

architecture xqszx of yioucneoq is
  
begin
  -- Single-driven assignments
  ecqf <= ecqf;
end xqszx;



-- Seed after: 14861050234293870041,6299883410057943775
