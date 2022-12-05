package com.github.ephemient.aoc2022.graalvm;

import org.graalvm.nativeimage.ImageInfo;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assumptions.assumeTrue;

public class ImageInfoTest {
    @Test
    public void isGraalImage() {
        assumeTrue(ImageInfo.inImageCode());
    }
}
