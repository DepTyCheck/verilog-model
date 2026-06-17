-- Seed: 12440122104918295815,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity bicgv is
  port (giyjdrdwq : in std_logic; xues : inout real);
end bicgv;

architecture anecnvkrqy of bicgv is
  
begin
  -- Single-driven assignments
  xues <= 2#1_1_1.0#;
end anecnvkrqy;

entity frzxfbia is
  port (bcuhchsbjq : out real);
end frzxfbia;

library ieee;
use ieee.std_logic_1164.all;

architecture eszmkenq of frzxfbia is
  signal k : real;
  signal gofcbdzrle : std_logic;
begin
  mikxhrslz : entity work.bicgv
    port map (giyjdrdwq => gofcbdzrle, xues => k);
  inmgemhd : entity work.bicgv
    port map (giyjdrdwq => gofcbdzrle, xues => bcuhchsbjq);
  
  -- Multi-driven assignments
  gofcbdzrle <= '-';
  gofcbdzrle <= 'U';
  gofcbdzrle <= 'L';
  gofcbdzrle <= 'H';
end eszmkenq;

library ieee;
use ieee.std_logic_1164.all;

entity srmbr is
  port (eivujfklc : buffer integer; lpsiipcrwo : out std_logic_vector(4 to 4); ypcqasvep : in integer; rycgde : out integer);
end srmbr;

library ieee;
use ieee.std_logic_1164.all;

architecture cttazcj of srmbr is
  signal vctkq : real;
  signal cwgbgbwo : real;
  signal q : std_logic;
begin
  jjcwgca : entity work.bicgv
    port map (giyjdrdwq => q, xues => cwgbgbwo);
  jgfmh : entity work.bicgv
    port map (giyjdrdwq => q, xues => vctkq);
  
  -- Single-driven assignments
  rycgde <= 8#60217#;
  eivujfklc <= 2#00#;
end cttazcj;

entity h is
  port (y : out real);
end h;

architecture sxgcurxfdg of h is
  
begin
  -- Single-driven assignments
  y <= 2#0001.10110#;
end sxgcurxfdg;



-- Seed after: 11938140306990728832,10557070023141912087
