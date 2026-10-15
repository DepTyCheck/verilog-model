-- Seed: 17016922436611442354,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity aoa is
  port (tyvscfa : linkage std_logic_vector(0 downto 4); awdvcovtvw : inout integer; gyyvrhpmdt : linkage string(1 to 1));
end aoa;

architecture vdcv of aoa is
  
begin
  -- Single-driven assignments
  awdvcovtvw <= 2#01001#;
end vdcv;

library ieee;
use ieee.std_logic_1164.all;

entity ech is
  port (iqia : out integer; zohbeuow : in std_logic; d : out real);
end ech;

library ieee;
use ieee.std_logic_1164.all;

architecture qiigfjkx of ech is
  signal jsxwx : string(1 to 1);
  signal tg : integer;
  signal dnoffukwb : string(1 to 1);
  signal zkziqmzlgo : string(1 to 1);
  signal yffrgddc : integer;
  signal zrusu : std_logic_vector(0 downto 4);
  signal ckqluub : string(1 to 1);
  signal hzymizztl : integer;
  signal qxclbdhqn : std_logic_vector(0 downto 4);
begin
  feld : entity work.aoa
    port map (tyvscfa => qxclbdhqn, awdvcovtvw => hzymizztl, gyyvrhpmdt => ckqluub);
  atofu : entity work.aoa
    port map (tyvscfa => zrusu, awdvcovtvw => yffrgddc, gyyvrhpmdt => zkziqmzlgo);
  hyektp : entity work.aoa
    port map (tyvscfa => zrusu, awdvcovtvw => iqia, gyyvrhpmdt => dnoffukwb);
  inwncwsd : entity work.aoa
    port map (tyvscfa => qxclbdhqn, awdvcovtvw => tg, gyyvrhpmdt => jsxwx);
  
  -- Single-driven assignments
  d <= d;
  
  -- Multi-driven assignments
  zrusu <= (others => '0');
  qxclbdhqn <= qxclbdhqn;
  zrusu <= qxclbdhqn;
end qiigfjkx;



-- Seed after: 9871375143066798019,6377581586043967497
