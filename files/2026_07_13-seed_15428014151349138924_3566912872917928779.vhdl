-- Seed: 15428014151349138924,3566912872917928779

library ieee;
use ieee.std_logic_1164.all;

entity nn is
  port (ginwtfrc : out real; sx : out std_logic_vector(0 downto 0); yijiihe : buffer boolean_vector(4 to 4));
end nn;

architecture vas of nn is
  
begin
  -- Single-driven assignments
  yijiihe <= (others => TRUE);
  ginwtfrc <= ginwtfrc;
  
  -- Multi-driven assignments
  sx <= "0";
  sx <= "U";
  sx <= "L";
end vas;

use std.reflection.all;

entity guxmvonmi is
  port ( umox : inout array_value_mirror
  ; meua : inout enumeration_subtype_mirror
  ; yjfepuu : inout enumeration_value_mirror
  ; ypgtqzj : inout enumeration_value_mirror
  );
end guxmvonmi;

library ieee;
use ieee.std_logic_1164.all;

architecture iadfku of guxmvonmi is
  signal acmq : boolean_vector(4 to 4);
  signal dgnphp : real;
  signal xsrwyckfq : boolean_vector(4 to 4);
  signal cyripnzsx : std_logic_vector(0 downto 0);
  signal swoen : real;
  signal pz : boolean_vector(4 to 4);
  signal qzq : std_logic_vector(0 downto 0);
  signal mmoy : real;
  signal s : boolean_vector(4 to 4);
  signal xyzk : std_logic_vector(0 downto 0);
  signal yadkix : real;
begin
  xkkniyv : entity work.nn
    port map (ginwtfrc => yadkix, sx => xyzk, yijiihe => s);
  xjwneethv : entity work.nn
    port map (ginwtfrc => mmoy, sx => qzq, yijiihe => pz);
  mtuvpkikda : entity work.nn
    port map (ginwtfrc => swoen, sx => cyripnzsx, yijiihe => xsrwyckfq);
  iathzmqoro : entity work.nn
    port map (ginwtfrc => dgnphp, sx => xyzk, yijiihe => acmq);
end iadfku;

use std.reflection.all;

entity v is
  port (xqznemfw : inout integer_subtype_mirror; kttj : inout floating_value_mirror; qrggvd : inout access_value_mirror; c : in boolean);
end v;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture ouo of v is
  signal bsyagnxfvk : boolean_vector(4 to 4);
  signal bxjc : real;
  signal g : boolean_vector(4 to 4);
  signal bj : real;
  shared variable xwa : enumeration_value_mirror;
  shared variable fxm : enumeration_value_mirror;
  shared variable lijpfd : enumeration_subtype_mirror;
  shared variable dtxiptqw : array_value_mirror;
  signal zeay : boolean_vector(4 to 4);
  signal zgc : std_logic_vector(0 downto 0);
  signal hlrx : real;
begin
  bthtjtewvp : entity work.nn
    port map (ginwtfrc => hlrx, sx => zgc, yijiihe => zeay);
  pi : entity work.guxmvonmi
    port map (umox => dtxiptqw, meua => lijpfd, yjfepuu => fxm, ypgtqzj => xwa);
  xqcch : entity work.nn
    port map (ginwtfrc => bj, sx => zgc, yijiihe => g);
  y : entity work.nn
    port map (ginwtfrc => bxjc, sx => zgc, yijiihe => bsyagnxfvk);
  
  -- Multi-driven assignments
  zgc <= (others => '0');
  zgc <= (others => 'U');
  zgc <= zgc;
end ouo;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity tajntuk is
  port (jcphu : inout physical_value_mirror; jd : linkage time; iqdqviw : in integer; hfiazyazzq : inout std_logic);
end tajntuk;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture qaxg of tajntuk is
  shared variable rnbbnlrwqc : access_value_mirror;
  shared variable sydtfh : floating_value_mirror;
  shared variable bgv : integer_subtype_mirror;
  signal z : boolean;
  shared variable tg : access_value_mirror;
  shared variable arpr : floating_value_mirror;
  shared variable d : integer_subtype_mirror;
  signal epsmu : boolean_vector(4 to 4);
  signal leu : std_logic_vector(0 downto 0);
  signal waa : real;
begin
  aopmvfb : entity work.nn
    port map (ginwtfrc => waa, sx => leu, yijiihe => epsmu);
  fyknvnfjx : entity work.v
    port map (xqznemfw => d, kttj => arpr, qrggvd => tg, c => z);
  ebu : entity work.v
    port map (xqznemfw => bgv, kttj => sydtfh, qrggvd => rnbbnlrwqc, c => z);
  
  -- Single-driven assignments
  z <= z;
  
  -- Multi-driven assignments
  leu <= leu;
  leu <= "1";
  hfiazyazzq <= hfiazyazzq;
  hfiazyazzq <= hfiazyazzq;
end qaxg;



-- Seed after: 752786963409623526,3566912872917928779
