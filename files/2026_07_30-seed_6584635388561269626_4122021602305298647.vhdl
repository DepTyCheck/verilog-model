-- Seed: 6584635388561269626,4122021602305298647

entity wtxmkhdq is
  port (rjfosvu : buffer character; eenwpgq : in time_vector(4 to 0));
end wtxmkhdq;

architecture firhczaea of wtxmkhdq is
  
begin
  -- Single-driven assignments
  rjfosvu <= 'x';
end firhczaea;

entity ykfxi is
  port (m : linkage time);
end ykfxi;

architecture liftfgy of ykfxi is
  signal gsephdxcdl : time_vector(4 to 0);
  signal vhahgvg : character;
  signal cvoiaxdfl : time_vector(4 to 0);
  signal toe : character;
  signal zkcjsmpwcv : character;
  signal eilekzymhj : time_vector(4 to 0);
  signal icvaxyqkyb : character;
begin
  ysora : entity work.wtxmkhdq
    port map (rjfosvu => icvaxyqkyb, eenwpgq => eilekzymhj);
  ofrdsvrnlr : entity work.wtxmkhdq
    port map (rjfosvu => zkcjsmpwcv, eenwpgq => eilekzymhj);
  tf : entity work.wtxmkhdq
    port map (rjfosvu => toe, eenwpgq => cvoiaxdfl);
  xvl : entity work.wtxmkhdq
    port map (rjfosvu => vhahgvg, eenwpgq => gsephdxcdl);
  
  -- Single-driven assignments
  gsephdxcdl <= (others => 0 ns);
  cvoiaxdfl <= (others => 0 ns);
  eilekzymhj <= (others => 0 ns);
end liftfgy;



-- Seed after: 18039561039461477078,4122021602305298647
