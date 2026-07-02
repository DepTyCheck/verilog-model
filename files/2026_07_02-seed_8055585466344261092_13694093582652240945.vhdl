-- Seed: 8055585466344261092,13694093582652240945

entity xycp is
  port (pxdfhynmuy : buffer time; hqcoiy : linkage bit_vector(3 downto 3); oywasr : in time);
end xycp;

architecture glzjv of xycp is
  
begin
  -- Single-driven assignments
  pxdfhynmuy <= 2421 us;
end glzjv;

entity tnn is
  port (cwiz : buffer real);
end tnn;

architecture yutnssgf of tnn is
  signal zppnbgvx : time;
  signal aptsrlf : bit_vector(3 downto 3);
  signal fplmpars : bit_vector(3 downto 3);
  signal eiqb : time;
  signal douxxqnv : time;
  signal zlymftow : bit_vector(3 downto 3);
  signal pj : time;
begin
  h : entity work.xycp
    port map (pxdfhynmuy => pj, hqcoiy => zlymftow, oywasr => douxxqnv);
  qq : entity work.xycp
    port map (pxdfhynmuy => eiqb, hqcoiy => fplmpars, oywasr => pj);
  tkyganqbcg : entity work.xycp
    port map (pxdfhynmuy => douxxqnv, hqcoiy => aptsrlf, oywasr => zppnbgvx);
  
  -- Single-driven assignments
  cwiz <= 8#1.2_1#;
  zppnbgvx <= 34341 ms;
end yutnssgf;



-- Seed after: 12743897045251273594,13694093582652240945
