-- Seed: 3702139828320767628,13857275728440271305

entity z is
  port (ffx : out integer; otcmnmuj : inout bit_vector(1 to 4));
end z;

architecture ktoql of z is
  
begin
  -- Single-driven assignments
  otcmnmuj <= ('0', '1', '0', '1');
  ffx <= 2;
end ktoql;

entity osgjeie is
  port (wnldke : buffer boolean; kfavca : linkage integer; wulhic : linkage integer);
end osgjeie;

architecture wyloqs of osgjeie is
  signal jzboshyfm : bit_vector(1 to 4);
  signal zuiwiwap : integer;
  signal fs : bit_vector(1 to 4);
  signal wwh : integer;
  signal irdyam : bit_vector(1 to 4);
  signal jxoqmoiqc : integer;
  signal fgjkpl : bit_vector(1 to 4);
  signal gxhnvq : integer;
begin
  n : entity work.z
    port map (ffx => gxhnvq, otcmnmuj => fgjkpl);
  psgrl : entity work.z
    port map (ffx => jxoqmoiqc, otcmnmuj => irdyam);
  ej : entity work.z
    port map (ffx => wwh, otcmnmuj => fs);
  yardsi : entity work.z
    port map (ffx => zuiwiwap, otcmnmuj => jzboshyfm);
  
  -- Single-driven assignments
  wnldke <= TRUE;
end wyloqs;



-- Seed after: 16449962808071511186,13857275728440271305
