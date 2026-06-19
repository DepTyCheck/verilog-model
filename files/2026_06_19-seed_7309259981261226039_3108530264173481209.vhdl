-- Seed: 7309259981261226039,3108530264173481209

entity gjlo is
  port (fphrph : linkage bit_vector(0 to 2));
end gjlo;

architecture aazleq of gjlo is
  
begin
  
end aazleq;

entity rpy is
  port (iebjuoh : inout time; ianap : inout character; d : out real);
end rpy;

architecture erne of rpy is
  signal zzmzoz : bit_vector(0 to 2);
  signal z : bit_vector(0 to 2);
begin
  fuma : entity work.gjlo
    port map (fphrph => z);
  x : entity work.gjlo
    port map (fphrph => zzmzoz);
  
  -- Single-driven assignments
  iebjuoh <= 2#10101# us;
  ianap <= 'y';
  d <= 16#38.3_3_7#;
end erne;



-- Seed after: 17299695738067184516,3108530264173481209
